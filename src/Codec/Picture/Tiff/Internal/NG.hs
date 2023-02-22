{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
module Codec.Picture.Tiff.Internal.NG where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( (<$>), (<*>), pure )
#endif

import Control.DeepSeq( NFData(..) )
import Control.Monad( forM_, when, replicateM, )
import Data.Bits( (.&.), unsafeShiftR )
import Data.Binary( Binary( .. ) )
import Data.Binary.Get( Get
                      , getWord16le, getWord16be
                      , getWord32le, getWord32be
                      , bytesRead
                      , skip
                      , getByteString, runGetOrFail, runGet
                      )
import Data.Binary.Put( Put
                      , putWord16le, putWord16be
                      , putWord32le, putWord32be
                      , putByteString
                      )
import Data.Function( on )
import Data.List( sortBy, mapAccumL, intercalate )
import qualified Data.Vector as V
import qualified Data.ByteString as B
import Data.Int( Int32, Int64 )
import Data.Word( Word8, Word16, Word32 )
import GHC.Generics( Generic )

import Codec.Picture.Metadata.Exif
import Codec.Picture.Tiff.Internal.Types
import Control.Monad.Reader
import Codec.Picture.Tiff
import qualified Data.ByteString.Lazy as Lb
import Data.Bifunctor
import Data.Foldable (find)
import Codec.Picture
import qualified Data.Vector.Storable as VS
import Codec.Picture.Metadata (Metadatas)
import qualified Codec.Picture.Metadata as Met
import qualified Data.ByteString.Char8 as BC
import qualified Data.Foldable as F

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

{-import Debug.Trace-}

data PrimGet = PrimGet {
  getWord16 :: Get Word16
, getWord32 :: Get Word32
, getInt32 :: Get Int32
, primEndianness :: Endianness
}

mkPrimGet :: Endianness -> PrimGet
mkPrimGet e@EndianLittle = PrimGet {
    getWord16 = getWord16le
  , getWord32 = getWord32le
  , getInt32 = fromIntegral <$> getWord32le -- TODO this is probably wrong
  , primEndianness = e
  }
mkPrimGet e@EndianBig = PrimGet {
    getWord16 = getWord16be
  , getWord32 = getWord32be
  , getInt32 = fromIntegral <$> getWord32be -- TODO this is probably wrong
  , primEndianness = e
  }

newtype TiffReader a = TiffReader {
  unTiffReader :: ReaderT PrimGet Get a
} deriving (Functor, Applicative, Monad, MonadFail)

runGetEither :: Lb.ByteString -> Get a -> Either String a
runGetEither raw g = case runGetOrFail g raw of
    Left (_rest, _consumed, err) -> Left err
    Right (_rest, _consumed, res) -> Right res

runTiffReader :: Endianness -> Lb.ByteString -> TiffReader a -> Either String a
runTiffReader e str = runGetEither str . flip runReaderT (mkPrimGet e) . unTiffReader

findIFDData :: [(TiffTag, ExifData)] -> ExifTag -> Either String Word32
findIFDData tags tag = tagOffset . fst <$> findIFD tags tag

findIFDDefaultData :: [(TiffTag, ExifData)] -> Word32 -> ExifTag -> Word32
findIFDDefaultData tags d ifd = either (const d) id $ findIFDData tags ifd

findIFD :: [(TiffTag, ExifData)] -> ExifTag -> Either String (TiffTag, ExifData)
findIFD tags tag =
  case find ((== tag) . tagIdentifier . fst) tags of
    Nothing -> Left $ "Unable to find tag " <> show tag
    Just x -> pure x

findIFDExt ::  [(TiffTag, ExifData)] -> ExifTag -> Either String ExifData
findIFDExt tags tag = do
  (val, exif) <- findIFD tags tag
  case val of
    TiffTag
      { tagCount = 1, tagOffset = ofs, tagType = TypeShort } ->
              pure . ExifShorts . V.singleton $ fromIntegral ofs
    TiffTag
      { tagCount = 1, tagOffset = ofs, tagType = TypeLong } ->
              pure . ExifLongs  . V.singleton $ fromIntegral ofs
    _ -> pure exif

findIFDExtDefaultData :: [(TiffTag, ExifData)] -> [Word32] -> ExifTag -> Either String [Word32]
findIFDExtDefaultData tags d tag =
  case snd <$> find ((== tag) . tagIdentifier . fst) tags of
    Nothing -> pure d
    Just ExifNone -> return d
    Just e -> V.toList <$> unLong errorMessage e
        where errorMessage = "Can't parse tag " ++ show tag ++ " " ++ show e

unLong :: String -> ExifData -> Either String (V.Vector Word32)
unLong _ (ExifLong v)   = Right $ V.singleton v
unLong _ (ExifShort v)  = Right $ V.singleton (fromIntegral v)
unLong _ (ExifShorts v) = Right $ V.map fromIntegral v
unLong _ (ExifLongs v) = Right v
unLong errMessage _ = Left errMessage


tiffInfoSummary :: TiffInfo -> String
tiffInfoSummary tinfo = intercalate "\n" [
    "tiffHeader = " <> show (tiffHeader tinfo)
  , "tiffWidth = " <> show (tiffWidth tinfo)
  , "tiffHeight = " <> show (tiffHeight tinfo)
  , "tiffColorspace = " <> show (tiffColorspace tinfo)
  , "tiffSampleCount = " <> show (tiffSampleCount tinfo)
  , "tiffRowPerStrip = " <> show (tiffRowPerStrip tinfo)
  , "tiffPlaneConfiguration = " <> show (tiffPlaneConfiguration tinfo)
  , "tiffSampleFormat = " <> show (tiffSampleFormat tinfo)
  , "tiffBitsPerSample = " <> show (tiffBitsPerSample tinfo)
  , "tiffCompression = " <> show (tiffCompression tinfo)
  -- , "tiffStripSize = " <> show (tiffStripSize tinfo)
  -- , "tiffOffsets = " <> show (tiffOffsets tinfo)
  -- , "tiffPalette = " <> show (tiffPalette tinfo)
  , "tiffYCbCrSubsampling = " <> show (tiffYCbCrSubsampling tinfo)
  , "tiffExtraSample = " <> show (tiffExtraSample tinfo)
  , "tiffPredictor = " <> show (tiffPredictor tinfo)
  -- , "tiffMetadatas = " <> show (tiffMetadatas tinfo)
  ]

testToPNG :: FilePath -> FilePath -> IO ()
testToPNG fpIn fpOut = do
  raw <- Lb.readFile fpIn
  case readTiffInfo raw of
    Left err -> putStrLn $ "Error reading tiff info: " <> err
    Right tiffs -> do
      forM_ (zip [0, 1..] tiffs) $ \(i, (tags, info)) -> do
        putStrLn $ "Image " <> show i
        putStrLn $ "strips: " <> show (V.length $ tiffOffsets info) 
        mapM_ print $ filterLongIds tags
        putStrLn $ tiffInfoSummary info
        case unpack (Lb.toStrict raw) info of
          Left err -> putStrLn $ "Error unpacking tiff" <> err
          Right img -> void $ writeDynamicPng (fpOut <> "_" <> show i <> ".png") $ palettedToTrueColor img
  where
    filterLongIds = filter (not . (`elem` [TagStripOffsets, TagUnknown 37724, TagUnknown 34675, TagStripByteCounts]) . tagIdentifier . fst)

readTiffInfo :: Lb.ByteString -> Either String [([(TiffTag, ExifData)], TiffInfo)]
readTiffInfo raw = do
  (header :: TiffHeader) <- case runGetOrFail get raw of
    Left (_rest, _consumed, err) -> Left err
    Right (_rest, _consumed, res) -> Right res
  let mkTiff ifd = do
        tags <- fetchExtended (hdrEndianness header) raw $ sortBy (compare `on` (tagOffset)) $ ifdTags ifd
        tinfo <- mkTiffInfo header tags
        pure (tags, tinfo)
  mapM mkTiff =<< readAllIFDs header raw

mkTiffInfo :: TiffHeader -> [(TiffTag, ExifData)] -> Either String TiffInfo
mkTiffInfo header tags = do
  let dataFind tag = findIFDData tags tag
      dataDefault def tag = pure $ findIFDDefaultData tags def tag
      extFind = findIFDExt tags
      extDefault = findIFDExtDefaultData tags
  TiffInfo header
    <$> dataFind TagImageWidth
    <*> dataFind TagImageLength
    <*> (dataFind TagPhotometricInterpretation
                >>= unpackPhotometricInterpretation')
    <*> dataFind TagSamplesPerPixel
    <*> dataFind TagRowPerStrip
    <*> (dataDefault 1 TagPlanarConfiguration
                >>= planarConfgOfConstant')
    <*> (extDefault [1] TagSampleFormat
                >>= mapM unpackSampleFormat')
    <*> (extFind TagBitsPerSample
                >>= unLong "Can't find bit depth")
    <*> (dataFind TagCompression
                >>= unPackCompression')
    <*> (extFind TagStripByteCounts
                >>= unLong "Can't find bit per sample")
    <*> (extFind TagStripOffsets
                >>= unLong "Can't find strip offsets")
    <*> pure (findPalette' tags)
    <*> (V.fromList <$> extDefault [2, 2] TagYCbCrSubsampling)
    <*> pure Nothing
    <*> (dataDefault 1 TagPredictor
                >>= predictorOfConstant')
    <*> pure (extractTiffMetadata' tags)

-- | Read all IFDs from a TIFF file.
readAllIFDs :: TiffHeader -> Lb.ByteString -> Either String [IFD]
readAllIFDs header raw = go (hdrOffset header)
  where
    go :: Word32 -> Either String [IFD]
    go idx
      | idx == 0 = Right []
      | otherwise = do
          res <- readAt (hdrEndianness header) idx raw readImageFileDirectory'
          xs <- go (ifdNextOffset res)
          pure $ res:xs

-- | Run a 'TiffReader' on a 'Lb.ByteString' returning the parsed value or an error message.
readAt :: Endianness -> Word32 -> Lb.ByteString -> TiffReader a -> Either String a
readAt e i bs r = runTiffReader e bs $ skipTo (fromIntegral i) >> r

-- | Read a `Word16` from the TIFF file.
readWord16 :: TiffReader Word16
readWord16 = TiffReader $ ReaderT getWord16

-- | Read a `Word32` from the TIFF file.
readWord32 :: TiffReader Word32
readWord32 = TiffReader $ ReaderT getWord32

-- | Read a `Int32` from the TIFF file.
readInt32 :: TiffReader Int32
readInt32 = TiffReader $ ReaderT getInt32

-- | Read a `Word32` in LE from the TIFF file.
readWord32LE :: TiffReader Word32
readWord32LE = TiffReader $ ReaderT $ const $ getWord32le -- TODO this is probably wrong

-- | Read a `Int32` in LE from the TIFF file.
readInt32LE :: TiffReader Int32
readInt32LE = TiffReader $ ReaderT $ const $ fromIntegral <$> getWord32le -- TODO this is probably wrong

endianness :: TiffReader Endianness
endianness = TiffReader $ ReaderT $ pure . primEndianness


readImageFileDirectory' :: TiffReader IFD
readImageFileDirectory' = do
  count <- readWord16
  res <- replicateM (fromIntegral count) readTag
  offset <- readWord32 -- TODO what is this? was empty in old version
  pure $ IFD res offset

data TiffTag = TiffTag
  { tagIdentifier :: !ExifTag -- Word16
  , tagType       :: !IfdType -- Word16
  , tagCount      :: !Word32
  , tagOffset     :: !Word32
  }
  deriving (Eq, Show, Generic)

data IFD = IFD {
  ifdTags :: ![TiffTag]
, ifdNextOffset :: !Word32
} deriving (Eq, Show, Generic)
  

-- | Read a `TiffTag`, reads the tag identifier, type, count and offset
readTag :: TiffReader TiffTag
readTag = do
  ident <- tagOfWord16 <$> readWord16
  ty <- ifdTypeOfWord16 =<< readWord16
  count <- readWord32
  offset <- readWord32
  pure $ TiffTag { 
      tagIdentifier = ident
    , tagType = ty
    , tagCount = count
    , tagOffset = offset
    }
  where
    ifdTypeOfWord16 :: Word16 -> TiffReader IfdType
    ifdTypeOfWord16 v = case v of
      1  -> pure TypeByte
      2  -> pure TypeAscii
      3  -> pure TypeShort
      4  -> pure TypeLong
      5  -> pure TypeRational
      6  -> pure TypeSByte
      7  -> pure TypeUndefined
      8  -> pure TypeSignedShort
      9  -> pure TypeSignedLong
      10 -> pure TypeSignedRational
      11 -> pure TypeFloat
      12 -> pure TypeDouble
      _  -> fail "Invalid TIF tag type"
    byOffset = sortBy (compare `on` ifdOffset)
    -- cleanIfds = fmap (cleanImageFileDirectory _endian)

skip' :: Int -> TiffReader ()
skip' = TiffReader . ReaderT . const . skip

getByteString' :: Int -> TiffReader B.ByteString
getByteString' i = TiffReader . ReaderT . const $ getByteString i

bytesRead' :: TiffReader Int64
bytesRead' = TiffReader . ReaderT $ const bytesRead

skipTo :: Int -> TiffReader ()
skipTo i = do
  loc <- bytesRead'
  skip' $ i - fromIntegral loc

cleanImageFileDirectory :: Endianness -> TiffTag -> TiffTag
cleanImageFileDirectory EndianBig tag@(TiffTag { tagCount = 1, tagType = TypeShort }) =
  tag { tagOffset = tagOffset tag `unsafeShiftR` 16 }
cleanImageFileDirectory _ tag = tag

readExifData :: Endianness -> Lb.ByteString -> TiffTag -> Either String ExifData
readExifData endianness' tif tag =
  case tag of
    (TiffTag { tagIdentifier = TagExifOffset , tagType = TypeLong , tagCount = 1 }) -> do
      subIfds <- readFromTiff $ do
        -- TODO might not need to sort here
        let byOffset = sortBy (compare `on` tagOffset)
            cleansIfds = fmap (cleanImageFileDirectory endianness')
        cleansIfds . byOffset . ifdTags <$> readImageFileDirectory'
      cleaned <- fetchExtended endianness' tif $ sortBy (compare `on` tagOffset) subIfds
      pure $ ExifIFD $ first tagIdentifier <$> cleaned
        {-  
    (ImageFileDirectory { ifdIdentifier = TagGPSInfo
                                , ifdType = TypeLong
                                , ifdCount = 1 } = do
        align 
        subIfds <- fmap (cleanImageFileDirectory endianness) <$> getP endianness
        cleaned <- fetchExtended endianness subIfds
        pure $ ExifIFD [(ifdIdentifier fd, ifdExtended fd) | fd <- cleaned]
      -}
    (TiffTag { tagType = TypeUndefined, tagCount = count }) | count > 4 ->
        readFromTiff $ ExifUndefined <$> getByteString' (fromIntegral count)
    (TiffTag { tagType = TypeUndefined, tagOffset = ofs }) ->
        pure . ExifUndefined . B.pack $ take (fromIntegral $ tagCount tag) (immediateBytes ofs)
    (TiffTag { tagType = TypeAscii, tagCount = count }) | count > 4 ->
        readFromTiff $ ExifString <$> getByteString' (fromIntegral count)
    (TiffTag { tagType = TypeAscii, tagOffset = ofs }) ->
        pure . ExifString . B.pack $ take (fromIntegral $ tagCount tag) (immediateBytes ofs)
    (TiffTag { tagType = TypeShort, tagCount = 2, tagOffset = ofs }) ->
        pure . ExifShorts $ V.fromListN 2 valList
          where high = fromIntegral $ ofs `unsafeShiftR` 16
                low = fromIntegral $ ofs .&. 0xFFFF
                valList = case endianness' of
                  EndianLittle -> [low, high]
                  EndianBig -> [high, low]
    (TiffTag { tagType = TypeRational, tagCount = 1 }) -> do
        readFromTiff $ ExifRational <$> readWord32LE <*> readWord32LE -- TODO check if this is correct
    (TiffTag { tagType = TypeSignedRational, tagCount = 1 }) -> do
        readFromTiff $ ExifSignedRational <$> readInt32LE <*> readInt32LE -- TODO check if this is correct
    (TiffTag { tagType = TypeShort, tagCount = 1 }) ->
        pure . ExifShort . fromIntegral $ tagOffset tag
    (TiffTag { tagType = TypeShort, tagCount = count }) | count > 2 ->
        readFromTiff $ ExifShorts <$> getVec count readWord16
    (TiffTag { tagType = TypeLong, tagCount = 1 }) ->
        pure . ExifLong . fromIntegral $ tagOffset tag
    (TiffTag { tagType = TypeLong, tagCount = count }) | count > 1 ->
        readFromTiff $ ExifLongs <$> getVec count readWord32
    _ -> pure ExifNone
  where
    readFromTiff = readAt endianness' (tagOffset tag) tif 
    getVec count = V.replicateM (fromIntegral count)

    immediateBytes ofs =
      let bytes = [fromIntegral $ (ofs .&. 0xFF000000) `unsafeShiftR` (3 * 8)
                  ,fromIntegral $ (ofs .&. 0x00FF0000) `unsafeShiftR` (2 * 8)
                  ,fromIntegral $ (ofs .&. 0x0000FF00) `unsafeShiftR` (1 * 8)
                  ,fromIntegral $  ofs .&. 0x000000FF
                  ]
      in case endianness' of
            EndianLittle -> reverse bytes
            EndianBig    -> bytes

fetchExtended :: Endianness -> Lb.ByteString -> [TiffTag] -> Either String [(TiffTag, ExifData)]
fetchExtended endian tif = mapM $ \tag -> (tag, ) <$> readExifData endian tif tag


unpackPhotometricInterpretation' :: Word32 -> Either String TiffColorspace
unpackPhotometricInterpretation' v = case v of
  0 -> Right TiffMonochromeWhite0
  1 -> Right TiffMonochrome
  2 -> Right TiffRGB
  3 -> Right TiffPaleted
  4 -> Right TiffTransparencyMask
  5 -> Right TiffCMYK
  6 -> Right TiffYCbCr
  8 -> Right TiffCIELab
  vv -> Left $ "Unrecognized color space " ++ show vv

planarConfgOfConstant' :: Word32 -> Either String TiffPlanarConfiguration
planarConfgOfConstant' 0 = Right PlanarConfigContig
planarConfgOfConstant' 1 = Right PlanarConfigContig
planarConfgOfConstant' 2 = Right PlanarConfigSeparate
planarConfgOfConstant' v = Left $ "Unknown planar constant (" ++ show v ++ ")"

unpackSampleFormat' :: Word32 -> Either String TiffSampleFormat
unpackSampleFormat' v = case v of
  1 -> Right TiffSampleUint
  2 -> Right TiffSampleInt
  3 -> Right TiffSampleFloat
  4 -> Right TiffSampleUnknown
  vv -> Left $ "Undefined data format (" ++ show vv ++ ")"

unPackCompression' :: Word32 -> Either String TiffCompression
unPackCompression' v = case v of
  0 -> Right CompressionNone
  1 -> Right CompressionNone
  2 -> Right CompressionModifiedRLE
  5 -> Right CompressionLZW
  6 -> Right CompressionJPEG
  7 -> Right CompressionJPEGNew
  32773 -> Right CompressionPackBit
  vv -> Left $ "Unknown compression scheme " ++ show vv

predictorOfConstant' :: Word32 -> Either String Predictor
predictorOfConstant' 1 = Right PredictorNone
predictorOfConstant' 2 = Right PredictorHorizontalDifferencing
predictorOfConstant' v = Left $ "Unknown predictor (" ++ show v ++ ")"

findPalette' :: [(TiffTag, ExifData)] -> Maybe (Image PixelRGB16)
findPalette' ifds =
    case [v | v <- ifds, tagIdentifier (fst v) == TagColorMap] of
        ((_, ExifShorts vec):_) ->
            Just . Image pixelCount 1 $ VS.generate (V.length vec) axx
                where pixelCount = V.length vec `div` 3
                      axx v = vec `V.unsafeIndex` (idx + color * pixelCount)
                          where (idx, color) = v `divMod` 3
        _ -> Nothing

extractTiffMetadata' :: [(TiffTag, ExifData)] -> Metadatas
extractTiffMetadata' lst = extractTiffDpiMetadata' lst <> extractTiffStringMetadata' lst

data TiffResolutionUnit'
  = ResolutionUnitUnknown'
  | ResolutionUnitInch'
  | ResolutionUnitCentimeter'

unitOfIfd' :: TiffTag -> TiffResolutionUnit'
unitOfIfd' tag = case (tagType tag, tagOffset tag) of
  (TypeShort, 1) -> ResolutionUnitUnknown'
  (TypeShort, 2) -> ResolutionUnitInch'
  (TypeShort, 3) -> ResolutionUnitCentimeter'
  _ -> ResolutionUnitUnknown'

extractTiffDpiMetadata' :: [(TiffTag, ExifData)] -> Metadatas
extractTiffDpiMetadata' lst = go where
  go = case unitOfIfd' <$> find ((== TagResolutionUnit) . tagIdentifier) (fst <$> lst) of
    Nothing -> mempty
    Just ResolutionUnitUnknown' -> mempty
    Just ResolutionUnitCentimeter' -> findDpis Met.dotsPerCentiMeterToDotPerInch mempty
    Just ResolutionUnitInch' -> findDpis id mempty

  findDpis toDpi =
     findDpi Met.DpiX TagXResolution toDpi . findDpi Met.DpiY TagYResolution toDpi

  findDpi k tag toDpi metas = case find ((== tag) . tagIdentifier . fst) lst of
    Nothing -> metas
    Just (_, ExifRational num den) ->
      Met.insert k (toDpi . fromIntegral $ num `div` den) metas
    Just _ -> metas

extractTiffStringMetadata' :: [(TiffTag, ExifData)] -> Metadatas
extractTiffStringMetadata' = Met.insert Met.Format Met.SourceTiff . foldMap go where
  strMeta k = Met.singleton k . BC.unpack
  exif' (tag, e) =
    Met.singleton (Met.Exif $ tagIdentifier tag) $ e
  inserter acc (k, v) = Met.insert (Met.Exif k) v acc
  exifShort tag =
    Met.singleton (Met.Exif $ tagIdentifier tag) . (ExifShort . fromIntegral) $ tagOffset tag

  go :: (TiffTag, ExifData) -> Metadatas
  go (tag, e) = case (tagIdentifier tag, e) of
    (TagArtist, ExifString v) -> strMeta Met.Author v
    (TagBitsPerSample, _) -> mempty
    (TagColorMap, _) -> mempty
    (TagCompression, _) -> mempty
    (TagCopyright, ExifString v) -> strMeta Met.Copyright v
    (TagDocumentName, ExifString v) -> strMeta Met.Title v
    (TagExifOffset, ExifIFD lst) -> F.foldl' inserter mempty lst
    (TagImageDescription, ExifString v) -> strMeta Met.Description v
    (TagImageLength, _) -> Met.singleton Met.Height . fromIntegral $ tagOffset tag
    (TagImageWidth, _) -> Met.singleton Met.Width . fromIntegral $ tagOffset tag
    (TagJPEGACTables, _) -> mempty
    (TagJPEGDCTables, _) -> mempty
    (TagJPEGInterchangeFormat, _) -> mempty
    (TagJPEGInterchangeFormatLength, _) -> mempty
    (TagJPEGLosslessPredictors, _) -> mempty
    (TagJPEGPointTransforms, _) -> mempty
    (TagJPEGQTables, _) -> mempty
    (TagJPEGRestartInterval, _) -> mempty
    (TagJpegProc, _) -> mempty
    (TagModel, v) -> Met.singleton (Met.Exif TagModel) v
    (TagMake, v) -> Met.singleton (Met.Exif TagMake) v
    (TagOrientation, _) -> exifShort tag
    (TagResolutionUnit, _) -> mempty
    (TagRowPerStrip, _) -> mempty
    (TagSamplesPerPixel, _) -> mempty
    (TagSoftware, ExifString v) -> strMeta Met.Software v
    (TagStripByteCounts, _) -> mempty
    (TagStripOffsets, _) -> mempty
    (TagTileByteCount, _) -> mempty
    (TagTileLength, _) -> mempty
    (TagTileOffset, _) -> mempty
    (TagTileWidth, _) -> mempty
    (TagUnknown _, _) -> exif' (tag, e)
    (TagXResolution, _) -> mempty
    (TagYCbCrCoeff, _) -> mempty
    (TagYCbCrPositioning, _) -> mempty
    (TagYCbCrSubsampling, _) -> mempty
    (TagYResolution, _) -> mempty
    _ -> mempty