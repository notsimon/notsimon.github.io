import qualified Codec.Archive.Tar as Tar
import qualified Data.ByteString.Lazy as BS
import Hakyll.Core.Item
import Hakyll.Core.Identifier
import System.FilePath

entriesToItems :: Tar.Entries Tar.FormatError -> [Item BS.ByteString]
entriesToItems (Tar.Next entry tail) = case Tar.entryContent entry of
    Tar.NormalFile content _ -> (Item (fromFilePath $ Tar.entryPath entry) content) : (entriesToItems tail)
    otherwise -> entriesToItems tail
entriesToItems Tar.Done = []
entriesToItems (Tar.Fail e) = error "Unvalid tarball"

prependPath :: FilePath -> Item a -> Item a
prependPath path (Item identifier content) = 
    Item (fromFilePath (path </> (toFilePath identifier))) content

main = do
    let route = "css/fonts.tar"
    content <- BS.readFile "css/fonts.tar"
    let entries = Tar.read content
    let items = fmap (prependPath $ takeDirectory route) $ entriesToItems entries
    mapM (\e -> print . show $ itemIdentifier e) items
