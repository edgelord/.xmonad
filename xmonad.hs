{-# LANGUAGE AutoDeriveTypeable    #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LiberalTypeSynonyms   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}

import qualified Data.Map                      as M
import           Data.Monoid
import           Data.Ratio ((%))
import           Data.List (isInfixOf)
  
import Graphics.X11.ExtraTypes.XF86
import System.Exit(ExitCode(ExitSuccess), exitWith)
                 
import           XMonad
import           XMonad.Actions.FloatSnap
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.Place
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.BoringWindows
import qualified XMonad.Layout.Fullscreen      as F
import           XMonad.Layout.Groups.Examples
import           XMonad.Layout.NoBorders
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Reflect
import           XMonad.Layout.Tabbed
import           XMonad.Layout.IM
import           XMonad.Layout.Minimize
import           XMonad.Prompt
import           XMonad.Prompt.Man
import           XMonad.Prompt.Shell
import           XMonad.Prompt.Window
import qualified XMonad.StackSet               as W
import           XMonad.Util.Cursor
import           XMonad.Util.EZConfig

import           Control.Concurrent            (forkIO, threadDelay)
import           Control.Monad                 (void)
import           System.IO                     (hFlush, stdout)

--------------------------------------------------------------------------------
----------------------------------- Commands -----------------------------------
--------------------------------------------------------------------------------

-- | Launch XMonad
main :: IO ()
-- main = xmonad myConfig
main = fixPanel >> xmonad myConfig


fixPanel :: IO ()
fixPanel = void $ forkIO $ do
  spawn "compton -f -I 0.10 -O 0.10 --backend glx --vsync opengl"
  spawn "emacs --daemon"
  spawn "xcape"
  spawn "notify-send 'Xmonad Started/Recompiled' --icon=emblem-system"

myConfig = ewmh $ def { borderWidth        = 1
                      , normalBorderColor  = "black"
                      , focusedBorderColor = "purple"
                      , terminal           = "termite"
                      , focusFollowsMouse  = False
                      , clickJustFocuses   = True
                      , modMask            = mod4Mask
                      , keys               = myKeys
                      , mouseBindings      = myMouse
                      , workspaces         = myWorkspaces
                      , layoutHook         = myLayout
                      , logHook            = myLogHook
                      , startupHook        = myStartupHook
                      , handleEventHook    = myHandleEventHook
                      , manageHook         = myManageHook
                      }

myXPConfig  :: XPConfig
myXPConfig  = def {
        -- font = "-*-avant garde gothic-demi-r-*-*"
        -- font = "xft:Profont:pixelsize=15:autohint=true"
        -- ,   height = 30
        -- more fuzzy searching, match anywhere
            searchPredicate = isInfixOf
        -- always highlight a result, so I can hit enter any time
        ,   alwaysHighlight = True
    }

-- | Separated from myKeymap so we can do a validity check at startup
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys cfg = mkKeymap cfg (myKeymap cfg)

-- | Key bindings
--   Quick guide: C = control
--              , M = alt
--              , S = shift
--              , M4 = super
--   A complete list of key names and other information
--   is available at the bottom of this file.
myKeymap :: XConfig Layout -> [(String, X ())]
myKeymap cfg = [ ("M4-S-<Return>",   startTerminal)
               , ("M4-S-k",          closeFocused)
               , ("M4-<Space>",      nextLayout)
               , ("M4-S-<Space>",    resetLayout)
               -- , ("M4-n",            refresh)
               , ("M4-<Tab>",        focusDown)
               , ("M4-S-<Tab>",      focusUp)
               , ("M4-<Return>",     swapMaster)
               , ("M4-,",            shrinkMaster)
               , ("M4-.",            expandMaster)
               -- , ("M4-,",            incrementMaster)
               -- , ("M4-.",            decrementMaster)
               , ("M4-q",            restartXMonad)
               , ("M4-S-q",          logoutCmd)
               , ("M4-m",            withFocused minimizeWindow)
               , ("M4-n",            sendMessage RestoreNextMinimizedWin)
               , ("M4-1",            viewWS 1)
               , ("M4-2",            viewWS 2)
               , ("M4-3",            viewWS 3)
               , ("M4-4",            viewWS 4)
               , ("M4-5",            viewWS 5)
               , ("M4-6",            viewWS 6)
               , ("M4-7",            viewWS 7)
               , ("M4-8",            viewWS 8)
               , ("M4-0",            viewWS 0)
               , ("M4-S-c",          moveToWS 8)
               , ("M4-S-1",          moveToWS 1)
               , ("M4-S-2",          moveToWS 2)
               , ("M4-S-3",          moveToWS 3)
               , ("M4-S-4",          moveToWS 4)
               , ("M4-S-5",          moveToWS 5)
               , ("M4-S-6",          moveToWS 6)
               , ("M4-S-7",          moveToWS 7)
               , ("M4-S-8",          moveToWS 8)
               , ("M4-w",            viewMonitor 1)
               , ("M4-e",            viewMonitor 2)
               , ("M4-r",            viewMonitor 3)
               , ("M4-S-w",          moveToMonitor 1)
               , ("M4-S-e",          moveToMonitor 2)
               , ("M4-S-r",          moveToMonitor 3)
               , ("M1-M4-b",         toggleStruts)
               , ("M4-c",            chromiumCmd)
               , ("M4-M1-c",         chromiumCmd)
               , ("M1-M4-k",         conkerorCmd)
               , ("M1-h", markBoring)
               -- , ("M4-i",            increaseNMasterGroups)
               -- , ("M4-o",            decreaseNMasterGroups)
               , ("M1-M4-t",         teamspeakCmd)
               , ("M4-p",            dmenuCmd)
               , ("M1-M4-z",         dmenuCmd)
               , ("M4-S-j",            windowPromptGoto   myXPConfig)
               , ("M4-S-p",            windowPromptBring  myXPConfig)
               -- , ("M4-m",            mocCmd)
               , ("M4-s",            shellPrompt myXPConfig)
               -- , ("M4-m",            manPrompt defaultXPConfig)
               , ("M1-M4-m",         mocCmd)
               , ("M1-M4-e",         emacsCmd)
               , ("M1-M4-p",         pavucontrolCmd)
               , ("<XF86AudioPlay>", mocPlayPauseCmd)
               , ("M4--",            shrinkTile)
               , ("M4-=",            expandTile)
               -- , ("xF86XK_AudioRaiseVolume", spawn "pactl set-sink-volume 0 +1.5%")
               -- , ("xF86XK_AudioLowerVolume", spawn "pactl set-sink-volume 0 -- -1.5%")
               -- , ("xF86XK_AudioMute", spawn "pactl set-sink-mute 0 toggle")
                 ]
  where
    startTerminal   = spawn $ XMonad.terminal cfg
    closeFocused    = kill
    nextLayout      = sendMessage NextLayout
    resetLayout     = setLayout $ XMonad.layoutHook cfg
    focusDown       = windows W.focusDown
    focusUp         = windows W.focusUp
    focusMaster     = windows W.focusMaster
    swapMaster      = windows W.swapMaster
    swapDown        = windows W.swapDown
    swapUp          = windows W.swapUp
    shrinkMaster    = sendMessage Shrink
    expandMaster    = sendMessage Expand
    retileWindow    = withFocused $ windows . W.sink
    incrementMaster = sendMessage $ IncMasterN 1
    decrementMaster = sendMessage $ IncMasterN (-1)
    restartXMonad   =
      spawn $ unwords [ "if type xmonad; then"
                      , "xmonad --recompile && xmonad --restart;"
                      , "else"
                      , "xmessage xmonad not in PATH: \"$PATH\";"
                      , "fi"
                      ]
    doWorkspace f i = windows $ f $ XMonad.workspaces cfg !! (i - 1)
    doMonitor f i   = screenWorkspace i >>= flip whenJust (windows . f)
    viewWS          = doWorkspace W.greedyView
    moveToWS        = doWorkspace W.shift
    viewMonitor     = doMonitor W.view
    moveToMonitor   = doMonitor W.shift
    toggleStruts    = sendMessage ToggleStruts
    -- chromiumCmd     = spawn "google-chrome-stable"
    chromiumCmd     = spawn "surf"
    conkerorCmd     = spawn "conkeror"
    teamspeakCmd    = spawn "ts3client"
    dmenuCmd        = spawn "yeganesh -x | zsh"
    emacsCmd        = spawn "emacsclient -c -a emacs"
    mocCmd          = spawn "xfce4-terminal -x mocp"
    mocPlayPauseCmd = spawn "mocp -G"
    pavucontrolCmd  = spawn "pavucontrol"
    logoutCmd       = io $ exitWith ExitSuccess
    shrinkTile      = sendMessage MirrorShrink
    expandTile      = sendMessage MirrorExpand

-- | Mouse bindings
--   buttons: 1 = left, 2 = middle, 3 = right, 4 = scroll down, 5 = scroll up
--myMouse :: MonadIO m => [((KeyMask, Button), m ())]
myMouse :: t -> M.Map (KeyMask, Button) (Window -> X ())
myMouse _ =  M.fromList [ ((mod4Mask, button1), floatMove)
                        , ((mod4Mask, button3), resizeMove)
                        ]
  where
    floatMove w = do
      focus w
      mouseMoveWindow w
      snapMagicMove (Just 50) (Just 50) w
      --windows W.shiftMaster
    resizeMove w = do
      focus w
      mouseResizeWindow w
      snapMagicResize [R,D] (Just 50) (Just 50) w
      --windows W.shiftMaster

-- | My workspacesn
myWorkspaces :: [WorkspaceId]
myWorkspaces = [ "1"
               , "2"
               , "3"
               , "4"
               , "5"
               , "6"
               , "7"
               , "8"
               , "9"
               , "10"
               ]

-- | My window layouts
myLayout = modifyL unmodified
  where
    unmodified = emacs ||| Full
    --- Emacs driven
    emacs = withIM (1%2) (ClassName "Emacs") tiled
    chrome = withIM (1%2) (ClassName "google-chrome") tiled
    --- tabbed test
    tabs = tallTabs defaultTiledTabsConfig
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Mirror $ ResizableTall nmaster delta ratio []
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio   = 1/2
    -- Percent of screen to increment by when resizing panes
    delta   = 3/100
    -- Functions to run on the layout
    modifyL = minimize . boringWindows . smartBorders . avoidStruts

-- | My event logging hook
myLogHook :: X ()
myLogHook = return ()

-- | My event handling hook
myHandleEventHook :: Event -> X All
myHandleEventHook = fullscreenEventHook

-- | My startup hook
myStartupHook :: X ()
myStartupHook = do
  setWMName "LG3D"
  setDefaultCursor xC_left_ptr
  return ()
  checkKeymap def (myKeymap undefined)
  return ()

-- | The 'ManageHook' for my XMonad configuration
myManageHook :: ManageHook
myManageHook = composeAll [ dialogMH
                          , strutsMH
                          , fullscreenMH
                          , specialMH ]
  where
    dialogMH     = isDialog --> doCenterFloat   -- Float dialog boxes
    strutsMH     = manageDocks                  -- Avoid struts (e.g.: a panel)
    fullscreenMH = isFullscreen --> doFullFloat -- Fixes fullscreen windows
    specialMH    = composeAll $ map (uncurry applyProp) specialWindows

-- | This is a list of programs where XMonad's default behavior is not ideal.
specialWindows :: [(X11Query, ManageHook)]
specialWindows =
  [ (QClassName "7zFM",                                  doCenterFloat)
  , (QClassName "Arandr",                                doCenterFloat)
  , (QClassName "Avahi-discover",                        doCenterFloat)
  , (QClassName "bssh",                                  doCenterFloat)
  , (QClassName "bvnc",                                  doCenterFloat)
  , (QClassName "File-roller",                           doCenterFloat)
  , (QClassName "Gigolo",                                doCenterFloat)
  , (QClassName "Ghb",                                   doCenterFloat)
  , (QClassName "melt",                                  doCenterFloat)
  , (QClassName ".nm-connection-editor-wrapped",         doCenterFloat)
  , (QClassName "net-sf-openrocket-startup-Startup",     doCenterFloat)
  , (QClassName "net-technicpack-launcher-LauncherMain", doCenterFloat)
  , (QClassName "Ristretto",                             doCenterFloat)
  , (QClassName "Unetbootin",                            doCenterFloat)
  -- , (QAppName "Xfce4-terminal",                        placeHook (underMouse (1, 1)))
  , (QClassName "Xfce4-terminal",                        placeHook $ fixed (1,1))
  , (QClassName "surf",                                  placeHook $ fixed (0,1))
  , (QClassName "Xfce4-about",                           doCenterFloat)
  , (QClassName "Xfce4-accessibility-settings",          doCenterFloat)
  , (QClassName "Xfce4-appearance-settings",             doCenterFloat)
  , (QClassName "Xfce4-display-settings",                doCenterFloat)
  , (QClassName "Xfce4-keyboard-settings",               doCenterFloat)
  , (QClassName "Xfce4-mime-settings",                   doCenterFloat)
  , (QClassName "Xfce4-mouse-settings",                  doCenterFloat)
  , (QClassName "Xfce4-notifyd-config",                  doCenterFloat)
  , (QClassName "Xfce4-session-settings",                doCenterFloat)
  , (QClassName "Xfce4-taskmanager",                     doCenterFloat)
  , (QClassName "Xfce4-settings-manager",                doCenterFloat)
  , (QClassName "Zenity",                                doCenterFloat)
  , (QClassName "Wrapper-1.0",                           doFloat)
  , (QClassName "MPlayer",                               doFloat)
  , (QClassName "Gimp",                                  doFloat)
  , (QAppName   "IcedTea-Web Control Panel",             doFloat)
  , (QAppName   "Java Control Panel",                    doFloat)
  , (QAppName   "Policy Tool",                           doFloat)
  ]


--------------------------------------------------------------------------------
----------------------------------- Utility ------------------------------------
--------------------------------------------------------------------------------


-- | An X11 query.
--   Find the data for creating an 'X11Query' with the @xprop@ command.
--   For example:
--      @WM_CLASS(STRING) = "Chromium"@ in your @xprop@ output
--   gives you
--      @FWClassName "Chromium"@ as an 'X11Query'
--
data X11Query = QTitle     String        -- ^ The X11 window title
              | QAppName   String        -- ^ The X11 application name
              | QClassName String        -- ^ The X11 class name
              | QArbitrary String String -- ^ An arbitrary X property
              deriving (Eq, Show, Read)

-- | Declare that windows that match a given 'X11Query'
--   should execute the given 'ManageHook'.
applyProp :: X11Query -> ManageHook -> Query (Endo WindowSet)
applyProp q mh = queryX11 q --> mh

-- | Translate an 'X11Query' to the corresponding 'Query Bool'.
queryX11 :: X11Query -> Query Bool
queryX11 (QTitle       s) = title            =? s
queryX11 (QAppName     s) = appName          =? s
queryX11 (QClassName   s) = className        =? s
queryX11 (QArbitrary p s) = stringProperty p =? s


--------------------------------------------------------------------------------
-------------------------------- Documentation ---------------------------------
--------------------------------------------------------------------------------


-- Key masks

-- Find the values of M1 through M4 with xmodmap -pm
--   shift    Shift_L (0x32), Shift_R (0x3e)
--   lock
--   control  Control_L (0x25), Control_L (0x42), Control_R (0x69)
--   mod1     Alt_L (0x40), Alt_R (0x6c), Meta_L (0xcd)
--   mod2     Num_Lock (0x4d)
--   mod3
--   mod4     Super_L (0x85), Super_R (0x86), Super_L (0xce), Hyper_L (0xcf)
--   mod5     ISO_Level3_Shift (0x5c), Mode_switch (0xcb)

-- Key symbols

-- <Backspace>
-- <Tab>
-- <Return>
-- <Pause>
-- <Scroll_lock>
-- <Sys_Req>
-- <Print>
-- <Escape>, <Esc>
-- <Delete>
-- <Home>
-- <Left>, <L>
-- <Up>, <U>
-- <Right>, <R>
-- <Down>, <D>
-- <Page_Up>
-- <Page_Down>
-- <End>
-- <Insert>
-- <Break>
-- <Space>
-- <F1>-<F24>
-- <KP_Space>
-- <KP_Tab>
-- <KP_Enter>
-- <KP_F1>
-- <KP_F2>
-- <KP_F3>
-- <KP_F4>
-- <KP_Home>
-- <KP_Left>
-- <KP_Up>
-- <KP_Right>
-- <KP_Down>
-- <KP_Prior>
-- <KP_Page_Up>
-- <KP_Next>
-- <KP_Page_Down>
-- <KP_End>
-- <KP_Begin>
-- <KP_Insert>
-- <KP_Delete>
-- <KP_Equal>
-- <KP_Multiply>
-- <KP_Add>
-- <KP_Separator>
-- <KP_Subtract>
-- <KP_Decimal>
-- <KP_Divide>
-- <KP_0>-<KP_9>
-- <XF86ModeLock>
-- <XF86MonBrightnessUp>
-- <XF86MonBrightnessDown>
-- <XF86KbdLightOnOff>
-- <XF86KbdBrightnessUp>
-- <XF86KbdBrightnessDown>
-- <XF86Standby>
-- <XF86AudioLowerVolume>
-- <XF86AudioMute>
-- <XF86AudioRaiseVolume>
-- <XF86AudioPlay>
-- <XF86AudioStop>
-- <XF86AudioPrev>
-- <XF86AudioNext>
-- <XF86HomePage>
-- <XF86Mail>
-- <XF86Start>
-- <XF86Search>
-- <XF86AudioRecord>
-- <XF86Calculator>
-- <XF86Memo>
-- <XF86ToDoList>
-- <XF86Calendar>
-- <XF86PowerDown>
-- <XF86ContrastAdjust>
-- <XF86RockerUp>
-- <XF86RockerDown>
-- <XF86RockerEnter>
-- <XF86Back>
-- <XF86Forward>
-- <XF86Stop>
-- <XF86Refresh>
-- <XF86PowerOff>
-- <XF86WakeUp>
-- <XF86Eject>
-- <XF86ScreenSaver>
-- <XF86WWW>
-- <XF86Sleep>
-- <XF86Favorites>
-- <XF86AudioPause>
-- <XF86AudioMedia>
-- <XF86MyComputer>
-- <XF86VendorHome>
-- <XF86LightBulb>
-- <XF86Shop>
-- <XF86History>
-- <XF86OpenURL>
-- <XF86AddFavorite>
-- <XF86HotLinks>
-- <XF86BrightnessAdjust>
-- <XF86Finance>
-- <XF86Community>
-- <XF86AudioRewind>
-- <XF86XF86BackForward>
-- <XF86Launch0>-<XF86Launch9>, <XF86LaunchA>-<XF86LaunchF>
-- <XF86ApplicationLeft>
-- <XF86ApplicationRight>
-- <XF86Book>
-- <XF86CD>
-- <XF86Calculater>
-- <XF86Clear>
-- <XF86Close>
-- <XF86Copy>
-- <XF86Cut>
-- <XF86Display>
-- <XF86DOS>
-- <XF86Documents>
-- <XF86Excel>
-- <XF86Explorer>
-- <XF86Game>
-- <XF86Go>
-- <XF86iTouch>
-- <XF86LogOff>
-- <XF86Market>
-- <XF86Meeting>
-- <XF86MenuKB>
-- <XF86MenuPB>
-- <XF86MySites>
-- <XF86New>
-- <XF86News>
-- <XF86OfficeHome>
-- <XF86Open>
-- <XF86Option>
-- <XF86Paste>
-- <XF86Phone>
-- <XF86Q>
-- <XF86Reply>
-- <XF86Reload>
-- <XF86RotateWindows>
-- <XF86RotationPB>
-- <XF86RotationKB>
-- <XF86Save>
-- <XF86ScrollUp>
-- <XF86ScrollDown>
-- <XF86ScrollClick>
-- <XF86Send>
-- <XF86Spell>
-- <XF86SplitScreen>
-- <XF86Support>
-- <XF86TaskPane>
-- <XF86Terminal>
-- <XF86Tools>
-- <XF86Travel>
-- <XF86UserPB>
-- <XF86User1KB>
-- <XF86User2KB>
-- <XF86Video>
-- <XF86WheelButton>
-- <XF86Word>
-- <XF86Xfer>
-- <XF86ZoomIn>
-- <XF86ZoomOut>
-- <XF86Away>
-- <XF86Messenger>
-- <XF86WebCam>
-- <XF86MailForward>
-- <XF86Pictures>
-- <XF86Music>
-- <XF86TouchpadToggle>
-- <XF86_Switch_VT_1>-<XF86_Switch_VT_12>
-- <XF86_Ungrab>
-- <XF86_ClearGrab>
-- <XF86_Next_VMode>
-- <XF86_Prev_VMode>

-- Mouse symbols

-- button1 = left mouse button
-- button2 = middle mouse button
-- button3 = right mouse button
-- button4 = scroll down
-- button5 = scroll up

