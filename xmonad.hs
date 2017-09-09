import XMonad

import XMonad.Config.Mate

import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.HintedGrid
import XMonad.Layout.IndependentScreens
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing

import qualified XMonad.StackSet as W
import qualified Data.Map as M

import System.Environment (getEnvironment)

(.>) = flip (.)

swapLayout = sendMessage NextLayout

resetLayout conf
  = setLayout $ XMonad.layoutHook conf


moveWithWindow wsTag
  = W.shift wsTag .> W.view wsTag

doOnWs :: WorkspaceId -> X () -> X () -> X ()
doOnWs wsTag wsA otherA
  = do
    cur <- gets (W.currentTag . windowset)
    if cur == wsTag
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
  $ [ ( (modm, xK_f), sendMessage $ Toggle NBFULL)

    , ( (modm, xK_p), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"" )

    , ( (modm, xK_F2), mateRun )

    , ( (modm, xK_c), kill )


    ---
    -- WASD directions bindings
    ---

    -- ALT + WASD
    , ( (modm, xK_a), prevWS )
    , ( (modm, xK_d), nextWS )
    , ( (modm, xK_w), windows W.focusUp )
    , ( (modm, xK_s), windows W.focusDown )

    -- ALT + SHIFT + WASD
    -- W/S swap window but keep at on the same position
    , ( (modm .|. shiftMask, xK_a), shiftToPrev )
    , ( (modm .|. shiftMask, xK_d), shiftToNext )
    , ( (modm .|. shiftMask, xK_w), windows $ W.swapUp .> W.focusDown )
    , ( (modm .|. shiftMask, xK_s), windows $ W.swapDown .> W.focusUp )

    -- SUPER + WASD
    -- A/D change workspace with the window and reset layout to get a clear view
    , ( (mod4Mask, xK_a), shiftToPrev >> prevWS >> resetLayout conf )
    , ( (mod4Mask, xK_d), shiftToNext >> nextWS >> resetLayout conf )
    , ( (mod4Mask, xK_w), windows W.swapUp )
    , ( (mod4Mask, xK_s), windows W.swapDown )

    -- SUPER + SHIFT + WASD
    , ( (mod4Mask .|. shiftMask, xK_a), shiftPrevScreen )
    , ( (mod4Mask .|. shiftMask, xK_d), shiftNextScreen )
    -- "Toggle maximize": Switch layout then set current window to master
    , ( (mod4Mask .|. shiftMask, xK_w), swapLayout >> windows W.swapMaster )


    ---
    -- TAB, E or R
    ---

    -- ALT + (TAB, E or R)
    -- ALT + TAB is already "Move focus to the next window"
    , ( (modm, xK_e), toggleOrView "default" )
    , ( (modm, xK_r)
      , doOnWs "default" swapLayout
      $ resetLayout conf )

    -- ALT + SHIFT + (TAB, E or R)
    -- ALT + SHIFT + TAB is already "Move focus to the pevious window"
    -- ALT + SHIFT + R maximize on default, set as master on other (usefull mouse gesture)
    , ( (modm .|. shiftMask, xK_e), toggleOrDoSkip [] W.shift "default" )
    , ( (modm .|. shiftMask, xK_r)
      , doOnWs "default" swapLayout
      $ windows W.swapMaster )

    -- SUPER + (TAB, E or R)
    , ( (mod4Mask, xK_Tab), nextScreen )
    , ( (mod4Mask, xK_e)
      , toggleOrDoSkip [] moveWithWindow "default" >> resetLayout conf )
    , ( (mod4Mask, xK_r), resetLayout conf )

    -- SUPER + SHIFT + (TAB, E or R)
    , ( (mod4Mask .|. shiftMask, xK_Tab), swapNextScreen )
    ]

    ++

    -- SUPER + [1..9], move workspace with focused window
    [ ( (mod4Mask, key), windows (moveWithWindow wsTag) >> resetLayout conf )
      | (key, wsTag) <- zip [xK_1 .. xK_9] (XMonad.workspaces conf)
    ]


myWorkspaces = ["default", "browser", "debug", "code"] ++ map show [5..9]


myManageHook = composeAll
  [ manageDocks
  , isFullscreen --> doFullFloat ]


myLayout screencount

  -- Add a full screen mode
  = mkToggle1 NBFULL

  -- Add spacing for the panels
  $ avoidStruts

  $ onWorkspace "default" (mySpacing stackGrid ||| maximized)
  $ onWorkspace "code" (mySpacing mouseResizableWide ||| maximized)
  $ mySpacing mouseResizableDefault ||| maximized

  where

    maximized
      = noBorders Full

    stackGrid
      = GridRatio (4/3) False

    mouseResizableDefault
      = if screencount > 1
        then mouseResizableWide
        else mouseResizableTall

    mouseResizableWide
      = mouseResizableTall { isMirrored = True }

    --- For some reason only dragger a lot bigger than the spacing works well
    mouseResizableTall
      = mouseResizableTile { draggerType = FixedDragger 0 24 }

    mySpacing layout
      = smartBorders $ smartSpacingWithEdge 3 layout


myLogHook
  = logHook mateConfig
  >> updatePointer (0.5, 0.5) (1, 1)


myStartupHook screencount
  = startupHook mateConfig
  >> if screencount > 1
    then spawn
      "dconf write /org/mate/panel/general/toplevel-id-list \"['bottom', 'toplevel-2']\""
    else spawn
      "dconf write /org/mate/panel/general/toplevel-id-list \"['bottom']\""


main
  = do
    screencount <- countScreens
    xmonad $ myConfig screencount
