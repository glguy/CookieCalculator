import Distribution.MacOSX
import Distribution.Simple

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks {
         postBuild = appBundleBuildHook guiApps -- no-op if not MacOS X
       }

guiApps :: [MacApp]
guiApps = [MacApp "CookieClickerGtk"
                  Nothing
                  (Just "macos/Info.plist")
                  ["macos/perfectCookie.icns"]
                  [] -- No other binaries.
                  DoNotChase -- Try changing to ChaseWithDefaults
          ]
