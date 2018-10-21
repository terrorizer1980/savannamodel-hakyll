--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid ((<>))
import           Data.Maybe (fromMaybe)
import           Data.List (sortOn)
import           Text.Read (readMaybe)
import           System.FilePath (takeDirectory)
import           Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "semantic/dist/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "about.markdown" $ pageRules "page-about"

    match (complement "foods/**/metadata" .&&. "foods/**") $ do
        route $ setExtension "html"
        compile $ pandocCompiler
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
          loadAndApplyTemplate "templates/food-color-section.html" (foodColorSectionCtx i m)
       ) $ sortOn (\(_, m) ->  sortMetadata m) colorMetadata

foodColorSectionCtx :: Identifier -> Metadata -> Context String
foodColorSectionCtx i m
  =  constField "color-header" (fromMaybe ("Missing header in " ++ show i) $ lookupString "header" m)
  <> constField "color-class" (fromMaybe "pink" $ lookupString "css-class" m)
  <> listField "food-categories" defaultContext
     (loadFoodCategories (takeDirectory $ toFilePath i) "templates/food-category-section.html")
  <> defaultContext

loadFoodCategories :: FilePath -> Identifier -> Compiler [Item String]
loadFoodCategories d t = do
  let r = fromGlob $ d ++ "/*/metadata"
  categoryMetadata <- getAllMetadata r
  mapM (\(i, m) ->
          makeItem "" >>=
          loadAndApplyTemplate t (foodCategorySectionCtx i m)
       ) $ sortOn (\(_, m) -> sortMetadata m) categoryMetadata

foodCategorySectionCtx :: Identifier -> Metadata -> Context String
foodCategorySectionCtx i m = let d = takeDirectory $ toFilePath i
  in constField "section-header" (fromMaybe ("Missing category header in " ++ show i) $ lookupString "header" m)
  <> listField "section-category-foods" defaultContext
     (loadAll $ complement "foods/**/metadata" .&&. fromGlob (d ++ "/*"))
  <> listField "secondary-categories" defaultContext
     (loadFoodCategories (takeDirectory $ toFilePath i) "templates/food-category-section-partial.html")
  <> defaultContext

foodCtx :: Context String
foodCtx =
    constField "page-foods" "" <>
    defaultContext

sortMetadata :: Metadata -> Maybe Integer
sortMetadata m = lookupString "sort" m >>= readMaybe
