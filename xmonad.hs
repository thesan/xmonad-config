{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, MultiParamTypeClasses, TypeSynonymInstances #-}

-----------------------------------------------------------------------------
import XMonad

import XMonad.Config.Mate

import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer

-- import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.HintedGrid
import XMonad.Layout.IndependentScreens
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing

import qualified XMonad.StackSet as W
import qualified Data.Map as M

import System.Environment (getEnvironment)

(.>) = flip (.)


data NONE = NONE deriving (Read, Show, Eq, Typeable)
instance Transformer NONE Window where
  transform _ x k = k x (const x)

data MAXIMIZED = MAXIMIZED deriving (Read, Show, Eq, Typeable)
instance Transformer MAXIMIZED Window where
  transform _ x k = k (avoidStruts $ noBorders Full) (const x)


toggleLayout layout
  = withFocused (windows . W.sink) >> sendMessage (Toggle layout)

-- sendNoRefresh msg = do
--   w <- W.workspace . W.current <$> gets windowset
--   sendMessageWithNoRefresh msg w
--
-- useLayout conf layoutT
--   = setLayout (XMonad.layoutHook conf) >> toggleLayout layoutT
--
-- pickLayout n = gotoL n >> refresh where
--   gotoL n = foldr nextL (sendNoRefresh FirstLayout) [0..n-1]
--   nextL _ state = state >> sendNoRefresh NextLayout


toggleMaximise = toggleLayout MAXIMIZED

resetLayoutToggle = toggleLayout NONE


moveWithWindow wsTag
  = W.shift wsTag .> W.view wsTag

doOnWs :: [WorkspaceId] -> X () -> X () -> X ()
doOnWs wsTags wsA otherA
  = do
    cur <- gets (W.currentTag . windowset)
    if cur `elem` wsTags
      then wsA
      else otherA



myConfig screencount
  = mateConfig
  { terminal = "uxterm"
  , keys = myKeys screencount <+> keys mateConfig
  , workspaces = myWorkspaces

  , normalBorderColor  = "#000000"
  , focusedBorderColor = "#aaaaaa"

  , handleEventHook = docksEventHook >> handleEventHook mateConfig
  , layoutHook = myLayout screencount
  , logHook = myLogHook
  , manageHook = myManageHook
  , startupHook = myStartupHook screencount }



myKeys screencount conf@XConfig {modMask = modm}
  = M.fromList
  $ [ ( (mod4Mask, xK_f), toggleLayout NBFULL )

    , ( (mod4Mask .|. shiftMask, xK_f), toggleLayout NONE )

    , ( (modm, xK_p), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"" )

    , ( (modm, xK_F2), mateRun )

    , ( (modm, xK_c), kill )

    , ( (0, xK_F8), toggleWS )
    -- , ( (0, xK_F9), toggleOrView "default" )


    ---
    -- WASD directions bindings
    ---

    -- ALT + WASD
    -- W/S swap window but keep at on the same position
    , ( (modm, xK_a), shiftToPrev )
    , ( (modm, xK_d), shiftToNext )
    , ( (modm, xK_w), windows $ W.swapUp .> W.focusDown )
    , ( (modm, xK_s), windows $ W.swapDown .> W.focusUp )

    -- ALT + SHIFT + WASD
    , ( (modm .|. shiftMask, xK_a), shiftPrevScreen >> prevScreen )
    , ( (modm .|. shiftMask, xK_d), shiftNextScreen >> nextScreen )
    -- "Toggle maximize": Switch layout then set current window to master
    , ( (mod4Mask .|. shiftMask, xK_w), toggleMaximise >> windows W.swapMaster )

    -- SUPER + WASD
    , ( (mod4Mask, xK_a), prevWS )
    , ( (mod4Mask, xK_d), nextWS )
    , ( (mod4Mask, xK_w), windows W.focusUp )
    , ( (mod4Mask, xK_s), windows W.focusDown )

    -- SUPER + SHIFT + WASD
    -- A/D change workspace with the window and reset layout to get a clear view
    , ( (mod4Mask .|. shiftMask, xK_a), shiftToPrev >> prevWS >> resetLayoutToggle )
    , ( (mod4Mask .|. shiftMask, xK_d), shiftToNext >> nextWS >> resetLayoutToggle )
    , ( (mod4Mask .|. shiftMask, xK_w), windows W.swapUp )
    , ( (mod4Mask .|. shiftMask, xK_s), windows W.swapDown )


    ---
    -- TAB, E or R
    ---

    -- ALT + (TAB, E, R)
    -- , ( (modm, xK_Tab), windows (W.swapDown .> W.focusUp) >> resetLayoutToggle )
    , ( (modm, xK_Tab), nextScreen )
    , ( (modm, xK_e), doOnWs boardWS toggleMaximise resetLayoutToggle )
    , ( (modm, xK_r), toggleOrDoSkip [] W.shift "default" )
    -- , ( (modm, xK_grave), doOnWs boardWS toggleMaximise
    --   $ windows W.swapMaster >> resetLayoutToggle )

    -- ALT + SHIFT + (TAB, E, R `)
    -- ALT + SHIFT + TAB is already "Move focus to the pevious window"
    -- ALT + SHIFT + R maximize on default, set as master on other (usefull mouse gesture)
    -- , ( (modm .|. shiftMask, xK_Tab), windows W.focusUp >> resetLayoutToggle )
    , ( (modm .|. shiftMask, xK_Tab), swapNextScreen )
    , ( (modm .|. shiftMask, xK_e)
      , toggleOrDoSkip [] moveWithWindow "default" >> resetLayoutToggle
      )
    , ( (modm .|. shiftMask, xK_r), toggleLayout MAXIMIZED )
    -- , ( (modm .|. shiftMask, xK_r)
    --   , doOnWs boardWS toggleMaximise
    --     $ windows W.swapMaster >> resetLayoutToggle
    --   )

    -- SUPER + (TAB, E or R, `)
    , ( (mod4Mask, xK_Tab), windows W.focusDown )
    , ( (mod4Mask, xK_e), toggleLayout MAXIMIZED )
    , ( (mod4Mask, xK_r), toggleOrView "default" )
    , ( (mod4Mask, xK_grave), toggleLayout MAXIMIZED )

    -- SUPER + SHIFT + (TAB, E or R)
    , ( (mod4Mask .|. shiftMask, xK_Tab), windows W.focusUp )
    , ( (mod4Mask .|. shiftMask, xK_e), toggleLayout NONE )
    , ( (mod4Mask .|. shiftMask, xK_grave), toggleLayout NONE )

    , ( (mod4Mask, xK_Page_Up), toggleOrView "default" )
    , ( (mod4Mask, xK_Page_Down), toggleOrView "last" )
    ]

    ++
    -- SUPER + SHIFT + [1..9], move workspace with focused window
    -- ALT + [1..9], move workspace with focused window
    [ ( (mask, key), action wsTag )
        | (key, wsTag) <- zip [xK_1 .. xK_9] $ XMonad.workspaces conf
        , (mask, action) <-
            [ ( mod4Mask .|. shiftMask
              , \wsTag -> windows (moveWithWindow wsTag) >> resetLayoutToggle
              )
            , (mod4Mask, windows . W.greedyView)
            , (modm, windows . W.shift)
            ]
    ]


myWorkspaces = ["default", "debug", "code", "4", "5", "6", "board1", "board2", "last"]
boardWS = ["default", "board1", "board2"]


myManageHook = composeAll
  [ manageDocks
  , isFullscreen
    --> doFullFloat
  , className =? "Shutter"
    --> doCenterFloat
  ]


myLayout screencount
  = mkToggle (NONE ?? NBFULL ?? MAXIMIZED ?? EOT)
  $ avoidStruts
  $ smartBorders
  $ smartSpacingWithEdge 3
  $ stackGrid ||| tileWide ||| tileTall
  where
    stackGrid = GridRatio (4/3) False
    tileWide = tile { masterFrac = 9/10, isMirrored = True }
    tileTall = tile { masterFrac = 2/3 }
    tile = mouseResizableTile { draggerType = FixedDragger 0 24 }


myLogHook
  = logHook mateConfig
  >> updatePointer (0.5, 0.5) (1, 1)
  -- >> fadeInactiveCurrentWSLogHook 0.8


myStartupHook screencount
  = startupHook mateConfig
  >> if screencount > 1
    then spawn
      "dconf write /org/mate/panel/general/toplevel-id-list \"['bottom', 'toplevel-2']\""
      >> spawn "xsetwacom set 'Atmel Atmel maXTouch Digitizer' MapToOutput eDP1"
    else spawn
      "dconf write /org/mate/panel/general/toplevel-id-list \"['bottom']\""


main
  = do
    screencount <- countScreens
    xmonad $ myConfig screencount
