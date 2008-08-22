object HelpForm: THelpForm
  Left = 380
  Top = 188
  Width = 597
  Height = 602
  Caption = 'Help'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  PopupMenu = lphPopUp
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDeactivate = FormDeactivate
  PixelsPerInch = 96
  TextHeight = 13
  object ViewerPanel: TPanel
    Left = 0
    Top = 29
    Width = 589
    Height = 546
    Align = alClient
    BevelOuter = bvNone
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 0
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 589
    Height = 29
    Caption = 'ToolBar1'
    Images = aMainForm.ImageList1
    TabOrder = 1
    object ToolButton2: TToolButton
      Left = 0
      Top = 2
      Width = 8
      Caption = 'ToolButton2'
      ImageIndex = 1
      Style = tbsSeparator
    end
    object tbPrint: TToolButton
      Left = 8
      Top = 2
      Action = hlpPrint
    end
    object ToolButton3: TToolButton
      Left = 31
      Top = 2
      Width = 8
      Caption = 'ToolButton3'
      ImageIndex = 1
      Style = tbsSeparator
    end
    object tbOpen: TToolButton
      Left = 39
      Top = 2
      Action = hlpOpen
    end
    object ToolButton5: TToolButton
      Left = 62
      Top = 2
      Width = 8
      Caption = 'ToolButton5'
      ImageIndex = 2
      Style = tbsSeparator
    end
    object tbPrev: TToolButton
      Left = 70
      Top = 2
      Hint = 'Previous Output Page'
      Action = hlpPrev
      ParentShowHint = False
      ShowHint = True
    end
    object tbNext: TToolButton
      Left = 93
      Top = 2
      Hint = 'Next Output Page'
      Action = hlpNext
      ParentShowHint = False
      ShowHint = True
    end
    object tbReload: TToolButton
      Left = 116
      Top = 2
      Hint = 'Reload Current File'
      Action = hlpReload
      ParentShowHint = False
      ShowHint = True
    end
  end
  object ActionList1: TActionList
    Left = 72
    Top = 64
    object hlpPrint: TAction
      Caption = 'Print'
      ImageIndex = 14
      ShortCut = 16464
      OnExecute = hlpPrintExecute
    end
    object hlpOpen: TAction
      Caption = 'Open'
      ImageIndex = 1
      OnExecute = hlpOpenExecute
    end
    object hlpPrev: TAction
      Caption = 'Previous'
      ImageIndex = 104
      OnExecute = hlpPrevExecute
    end
    object hlpNext: TAction
      Caption = 'Next'
      ImageIndex = 105
      OnExecute = hlpPrevExecute
    end
    object hlpReload: TAction
      Caption = 'Reload'
      ImageIndex = 106
      OnExecute = hlpReloadExecute
    end
    object hlpRepeatSearch: TAction
      Caption = 'hlpRepeatSearch'
      ShortCut = 122
      SecondaryShortCuts.Strings = (
        'ALT+F3')
      OnExecute = hlpRepeatSearchExecute
    end
    object hlpCopy: TAction
      Caption = 'Copy'
      ShortCut = 16451
      OnExecute = hlpCopyExecute
    end
  end
  object MainMenu1: TMainMenu
    Left = 24
    Top = 64
    object File1: TMenuItem
      Caption = 'File'
      Visible = False
      object Print1: TMenuItem
        Action = hlpPrint
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        ShortCut = 27
        OnClick = Exit1Click
      end
    end
    object Edit1: TMenuItem
      Caption = 'Edit'
      Visible = False
      object Copy1: TMenuItem
        Action = hlpCopy
      end
    end
  end
  object lphPopUp: TPopupMenu
    MenuAnimation = [maTopToBottom]
    Left = 120
    Top = 69
    object Print2: TMenuItem
      Action = hlpPrint
    end
    object Open1: TMenuItem
      Action = hlpOpen
    end
    object Previous1: TMenuItem
      Action = hlpPrev
    end
    object Next1: TMenuItem
      Action = hlpNext
    end
    object Reload1: TMenuItem
      Action = hlpReload
    end
    object Copy2: TMenuItem
      Action = hlpCopy
    end
  end
end
