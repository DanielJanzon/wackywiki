{-
  Usage: ./Migrate <path-to-files>

  This is a template program that can be used to migrate
  an existing wiki to neo4j. This program assumes the wiki
  to be migrated is stored as a bunch of files where the
  name of the file coincides with the name of the page,
  the content of a page with the content of a file and
  finally that all links are in CamelCase.

  The mechanics of the program is as follows:

    1. Create a list of all pages by scanning a directory
       for all files passing the pageFilter.

    2. Create a neo4j node for each page in the list.

    3. Open each file corresponding to a wiki page and
       extract all CamelCase links in it.

    4. Strip the list of links from links pointing to
       non-existing pages.

    5. Create a directed edge in neo4j from the current
       page to the linked page.
       
-}

{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import System.Directory
import Data.List
import qualified System.IO as SysIo
import System.Environment (getArgs)

import Database.Neo4j
import qualified Data.HashMap.Lazy as Map
import qualified Data.Text as T
import Data.Int (Int64)

-- Local imports
import qualified Wiki as Wiki

type PageName = T.Text
type PageLink = PageName
type PageContent = T.Text
type Page = (PageName, PageContent, [PageLink])
type PageList = [Page]

notDiffs p = not (isSuffixOf ".diffs" p)
notDots p = not (p `elem` [".", ".."])

pageFilter :: [String] -> [String]
pageFilter = filter (\x -> notDots x && notDiffs x) 

listPages :: FilePath -> IO [FilePath]
listPages = liftM pageFilter . getDirectoryContents

loadPage :: FilePath -> FilePath -> IO Page
loadPage directory filename = do
    let filePath = directory ++ "/" ++ filename
    handle <- SysIo.openFile filePath SysIo.ReadMode
    SysIo.hSetEncoding handle SysIo.latin1 
    content <- SysIo.hGetContents handle
    let pageName = T.pack filename
    let pageContent = T.pack content
    let pageLinks = map T.pack (Wiki.extractLinks content)
    --SysIo.hClose handle
    return (pageName, pageContent, pageLinks)


loadPages :: FilePath -> IO [Page]
loadPages baseDirectory = do
    files <- listPages baseDirectory 
    sequence $ map (loadPage baseDirectory) files

-- Default Neo4j port
port :: Database.Neo4j.Port
port = 7474

-- Default Neo4j host
host :: Database.Neo4j.Hostname
host = "localhost"

createWikiPage :: Page -> IO (PageName, [PageLink], Node)
createWikiPage (name, content, links) = withConnection host port $ do
   let node = Map.fromList ["name" |: name,
                            "content" |: content]
   nodeReference <- createNode node
   return (name, links, nodeReference)

-- From a list of wiki pages, create corresponding neo4j nodes.
createWikiPages :: [Page] -> IO [(PageName, [PageLink], Node)]
createWikiPages pages = sequence $ map createWikiPage pages 


getPageNode :: [(PageName, [PageLink], Node)] -> PageName -> Node
getPageNode ((name, links, node):pages) targetName =
    if name == targetName then node
    else getPageNode pages targetName

nodeExists :: [(PageName, [PageLink], Node)] -> PageName -> Bool
nodeExists [] _ = False
nodeExists ((name, _, _):pages) targetName = if name == targetName
                                             then True
                                             else nodeExists pages targetName

-- From a list of wiki pages, and a particular page, create
-- Neo4j relationships between that page and all pages it
-- links to.
createWikiLink :: [(PageName, [PageLink], Node)] -> (PageName, [PageLink], Node) -> IO [Relationship]
createWikiLink pages (_, links, fromNode) = withConnection host port $ do
    -- Filter out links to non-existing pages.
    let completeLinks = filter (nodeExists pages) links
    let toNodes = map (getPageNode pages) completeLinks
    -- What to put as edge metadata? Use Map.empty for now.
    sequence $ map (createRelationship "LINKS" Map.empty fromNode) toNodes

createWikiLinks :: [(PageName, [PageLink], Node)] -> IO [Relationship]
createWikiLinks pages = do
    rels <- sequence $ map (createWikiLink pages) pages
    return $ concat rels

main = do
    args <- getArgs
    let [wikiDir] = args
    putStrLn "loading pages..."
    pages <- loadPages wikiDir
    putStrLn "done"
    putStrLn "creating nodes..."
    nodes <- createWikiPages pages
    putStrLn "done"
    putStrLn "creating edges..."
    createWikiLinks nodes
    putStrLn "done"
