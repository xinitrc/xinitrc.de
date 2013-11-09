module Plugins.LogarithmicTagCloud(renderLogTagCloud) where

import           Hakyll.Web.Tags (Tags, renderTags)

import           Hakyll.Core.Compiler (Compiler)
import qualified Text.Blaze.Html5                 as H
import qualified Text.Blaze.Html5.Attributes      as A
import           Text.Blaze.Html.Renderer.String  (renderHtml)

import           Text.Blaze.Html                  (toHtml, toValue, (!))

import Text.Printf                                (printf)


renderLogTagCloud :: Double
                  -> Double
                  -> String
                  -> Tags
                  -> Compiler String
renderLogTagCloud minSize maxSize unit = renderTags makeLink unwords
    where
      makeLink tag url count min' max' = renderHtml $
                                         H.a ! A.style (toValue $ "font-size: " ++ size count min' max')
                                         ! A.href (toValue url)
                                         $ toHtml tag
      size count min' max' =
          let diff = (log (fromIntegral max') - log (fromIntegral min'))
              relative = (log (fromIntegral count) - log (fromIntegral min')) / diff
              size' = minSize + relative * (maxSize - minSize)
          in printf "%.2f" size' ++ unit
                                