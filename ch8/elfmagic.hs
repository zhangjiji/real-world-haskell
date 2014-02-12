import qualified Data.ByteString.Lazy as L

halfMagic :: L.ByteString -> Bool
halfMagic content = L.take 4 content == elfMagic
  where elfMagic = L.pack [0x7f, 0x45, 0x4c, 0x46]

isElfFile :: FilePath -> IO Bool
isElfFile filePath = do
  content <- L.readFile filePath
  print $ L.unpack (L.take 4 content)
  return (halfMagic content)

main = return isElfFile
