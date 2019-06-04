--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid ((<>))
import           Data.Maybe (fromMaybe)
import           Data.List (sortOn)
import           Text.Read (readMaybe)
import           System.FilePath (takeDirectory, takeFileName)
import           Hakyll
import Control.Monad (filterM)
import System.FilePath (splitDirectories)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "semantic/dist/**" $ do
        route   idRoute
        compile copyFileCompiler

    -- Dump github files in `/` directory
    match "github/*" $ do
      route $ customRoute (takeFileName . toFilePath)
      compile copyFileCompiler

    match "about.markdown" $ pageRules "page-about"

    match (complement "foods/**/metadata" .&&. "foods/**") $ do
      route $ setExtension "html"
      compile $ do

        -- Get color context
        color <- head <$> (getUnderlying >>= getCategory')
        mColorItem <- findColor color =<< loadAll "colors/sections/*"

        -- Get quotes context
        mFoodname <- getUnderlying >>= flip getMetadataField "foodname"
        quotes <- case mFoodname of
          Just foodname -> filterQuotes foodname =<< loadAll "quotes/*"
          Nothing -> return []
        let foodCtx =
                listField "quotes" defaultContext (sortItems "ord" quotes) <>
                maybe mempty (\c -> constField "color-section" (itemBody c)) mColorItem <>
                defaultContext

        pandocCompiler
          >>= loadAndApplyTemplate "templates/food.html"    foodCtx
          >>= loadAndApplyTemplate "templates/default.html" foodCtx
          >>= relativizeUrls

    match "foods.html" $ do
        route idRoute
        compile $ do
          let foodsCtx =
                constField "page-foods" "" <>
                listField "food-color-sections" defaultContext loadFoodTable <>
                defaultContext

          getResourceBody
            >>= applyAsTemplate foodsCtx
            >>= loadAndApplyTemplate "templates/default.html" foodsCtx
            >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            let indexCtx =
                    constField "page-home" "" <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler
    match "foods/**/metadata" $ compile templateCompiler
    match "quotes/*" $ compile getResourceBody
    match "colors/sections/*" $ compile getResourceBody

-- | Return only the color item specified in color metadata
findColor :: MonadMetadata m => String -> [Item a] -> m (Maybe (Item a))
findColor colorname colors =
  let f :: MonadMetadata m => String -> Item a -> m Bool
      f cn (Item i _) = do mc <- (getMetadataField i "color")
                           return (mc == (Just cn))
      head' (x:_) = Just x
      head' [] = Nothing
  in head' <$> filterM (f colorname) colors

-- | Obtain categories from a page. Modified from Hakyll function with the same name.
getCategory' :: MonadMetadata m => Identifier -> m [String]
getCategory' = return . tail' . splitDirectories . takeDirectory . toFilePath
  where tail' (_:xs) = xs; tail' _ = []


-- | Sort items by metadata string
sortItems :: MonadMetadata m => String -> [Item a] -> m [Item a]
sortItems s is = do
  is' <- mapM (\i -> do
                  ord <- getMetadataField (itemIdentifier i) s
                  return (ord,i)
              ) is
  return $ map snd $ sortOn (\(s',_) -> s') is'

-- | Return only quotes which are tagged with food name
filterQuotes :: MonadMetadata m => String -> [Item a] -> m [Item a]
filterQuotes foodname ids =
  let f :: MonadMetadata m => String -> Item a -> m Bool
      f fn (Item i _) = do tags <- getTags i
                           return $ elem fn tags
  in filterM (f foodname) ids

pageRules :: String -> Rules ()
pageRules pageLabel = do
      let ctx = constField pageLabel "" <>
                defaultContext

      route   $ setExtension "html"
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

loadFoodTable :: Compiler [Item String]
loadFoodTable = do
  colorMetadata <- getAllMetadata "foods/*/metadata"
  mapM (\(i, m) ->
          makeItem "" >>=
          loadAndApplyTemplate "templates/foods-color-section.html" (foodColorSectionCtx i m)
       ) $ sortOn (\(_, m) ->  sortMetadata m) colorMetadata

foodColorSectionCtx :: Identifier -> Metadata -> Context String
foodColorSectionCtx i m =
  let color = fromMaybe "pink" $ lookupString "css-class" m
  in constField "color-header" (fromMaybe ("Missing header in " ++ show i) $ lookupString "header" m)
  <> constField "color-class" color
  <> listField "food-categories" defaultContext
     (loadFoodCategories color (takeDirectory $ toFilePath i) "templates/food-category-section.html")
  <> defaultContext

loadFoodCategories :: String -> FilePath -> Identifier -> Compiler [Item String]
loadFoodCategories color d t = do
  let r = fromGlob $ d ++ "/*/metadata"
  categoryMetadata <- getAllMetadata r
  mapM (\(i, m) ->
          makeItem "" >>=
          (\item -> do
              let d' = takeDirectory $ toFilePath i
              mainFoodSection <- loadAll $ complement "foods/**/metadata" .&&. fromGlob (d' ++ "/*") :: Compiler [Item String]
              let mMainFoodSection = if null mainFoodSection then Nothing else Just mainFoodSection
              secondaryFoodSection <- (loadFoodCategories color (takeDirectory $ toFilePath i) "templates/food-category-section-partial.html") :: Compiler [Item String]
              let mSecondaryFoodSection = if null secondaryFoodSection then Nothing else Just secondaryFoodSection
              loadAndApplyTemplate t (foodCategorySectionCtx color i m mMainFoodSection mSecondaryFoodSection) item
          )
       ) $ sortOn (\(_, m) -> sortMetadata m) categoryMetadata

foodCategorySectionCtx :: String -> Identifier -> Metadata -> Maybe [Item String] -> Maybe [Item String] -> Context String
foodCategorySectionCtx color i m mFoodSection mSecondaryFoodSection =
  let primary = maybe mempty (listField "section-category-foods" defaultContext . return) mFoodSection
      secondary = maybe mempty (listField "secondary-categories" defaultContext . return) mSecondaryFoodSection
  in constField "section-header" (fromMaybe ("Missing category header in " ++ show i) $ lookupString "header" m)
  <> constField "css-class" color
  <> primary
  <> secondary
  <> defaultContext

sortMetadata :: Metadata -> Maybe Integer
sortMetadata m = lookupString "sort" m >>= readMaybe
