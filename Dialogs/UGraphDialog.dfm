object GraphDialog: TGraphDialog
  Left = 527
  Top = 364
  Width = 517
  Height = 303
  BorderIcons = [biSystemMenu]
  Caption = 'GraphDialog'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 509
    Height = 235
    ActivePage = tabSPC
    Align = alClient
    TabOrder = 0
    object tabVariables: TTabSheet
      Caption = 'Variables'
      object lblVar1: TLabel
        Left = 168
        Top = 16
        Width = 6
        Height = 13
        Caption = '_'
      end
      object lblVar2: TLabel
        Left = 168
        Top = 45
        Width = 6
        Height = 13
        Caption = '_'
      end
      object lblVar3: TLabel
        Left = 168
        Top = 74
        Width = 6
        Height = 13
        Caption = '_'
      end
      object Label8: TLabel
        Left = 8
        Top = 160
        Width = 37
        Height = 13
        Caption = 'Weight:'
      end
      object lblBy: TLabel
        Left = 168
        Top = 144
        Width = 6
        Height = 13
        Caption = '_'
      end
      object Label21: TLabel
        Left = 8
        Top = 120
        Width = 15
        Height = 13
        Caption = 'By:'
      end
      object lblWeight: TLabel
        Left = 168
        Top = 184
        Width = 6
        Height = 13
        Caption = '_'
      end
      object lblVar4: TLabel
        Left = 168
        Top = 104
        Width = 6
        Height = 13
        Caption = '_'
      end
      object cbVar1: TComboBox
        Tag = 1
        Left = 8
        Top = 8
        Width = 145
        Height = 21
        ItemHeight = 13
        TabOrder = 0
        OnSelect = ComboBoxSelect
      end
      object cbVar2: TComboBox
        Tag = 2
        Left = 8
        Top = 37
        Width = 145
        Height = 21
        ItemHeight = 13
        TabOrder = 1
        OnSelect = ComboBoxSelect
      end
      object cbVar3: TComboBox
        Tag = 3
        Left = 8
        Top = 66
        Width = 145
        Height = 21
        ItemHeight = 13
        TabOrder = 2
        OnSelect = ComboBoxSelect
      end
      object cbVar4: TComboBox
        Tag = 4
        Left = 8
        Top = 96
        Width = 145
        Height = 21
        ItemHeight = 13
        TabOrder = 3
        OnSelect = ComboBoxSelect
      end
      object cbBy: TComboBox
        Tag = 5
        Left = 8
        Top = 136
        Width = 145
        Height = 21
        ItemHeight = 13
        TabOrder = 4
        OnSelect = ComboBoxSelect
      end
      object cbWeight: TComboBox
        Tag = 6
        Left = 8
        Top = 176
        Width = 145
        Height = 21
        ItemHeight = 13
        TabOrder = 5
        OnSelect = ComboBoxSelect
      end
    end
    object tabSPC: TTabSheet
      Caption = 'SPC'
      ImageIndex = 2
      object GrpTests: TGroupBox
        Left = 8
        Top = 7
        Width = 164
        Height = 180
        Caption = 'Tests:'
        TabOrder = 0
        object Bevel1: TBevel
          Left = 16
          Top = 50
          Width = 131
          Height = 3
        end
        object Bevel2: TBevel
          Left = 88
          Top = 64
          Width = 0
          Height = 57
        end
        object Bevel3: TBevel
          Left = 16
          Top = 122
          Width = 131
          Height = 3
        end
        object chkTest1: TCheckBox
          Left = 16
          Top = 64
          Width = 61
          Height = 17
          Caption = 'Test 1'
          TabOrder = 2
          OnClick = chkTest1Click
        end
        object chkTest2: TCheckBox
          Left = 16
          Top = 82
          Width = 61
          Height = 17
          Caption = 'Test 2'
          TabOrder = 3
          OnClick = chkTest1Click
        end
        object chkTest3: TCheckBox
          Left = 16
          Top = 101
          Width = 61
          Height = 17
          Caption = 'Test 3'
          TabOrder = 4
          OnClick = chkTest1Click
        end
        object chkCheckAll: TCheckBox
          Left = 16
          Top = 24
          Width = 97
          Height = 17
          Caption = 'Check All'
          TabOrder = 0
          OnClick = chkCheckAllClick
        end
        object chkTest4: TCheckBox
          Left = 16
          Top = 135
          Width = 60
          Height = 17
          Caption = 'Test 4'
          TabOrder = 5
          OnClick = chkTest1Click
        end
        object chkTest5: TCheckBox
          Left = 16
          Top = 156
          Width = 60
          Height = 17
          Caption = 'Test 5'
          TabOrder = 6
          OnClick = chkTest1Click
        end
        object chkCombiTest: TCheckBox
          Left = 80
          Top = 64
          Width = 65
          Height = 17
          Caption = 'Test 1-3'
          TabOrder = 1
          OnClick = chkCombiTestClick
        end
      end
      object GrpBreaks: TGroupBox
        Left = 182
        Top = 7
        Width = 167
        Height = 122
        Caption = 'Breaks:'
        TabOrder = 1
        object brkLabel: TLabel
          Left = 8
          Top = 28
          Width = 9
          Height = 13
          Caption = '1:'
        end
        object Label4: TLabel
          Left = 8
          Top = 60
          Width = 9
          Height = 13
          Caption = '2:'
        end
        object Label7: TLabel
          Left = 8
          Top = 92
          Width = 84
          Height = 13
          Caption = 'Repeat breaks by'
        end
        object edBreak1: TMaskEdit
          Left = 32
          Top = 24
          Width = 118
          Height = 21
          TabOrder = 0
        end
        object edBreak2: TMaskEdit
          Left = 32
          Top = 56
          Width = 118
          Height = 21
          TabOrder = 1
        end
        object edBreakBy: TMaskEdit
          Left = 96
          Top = 88
          Width = 53
          Height = 21
          Hint = 'Break every '
          TabOrder = 2
        end
      end
      object GrpLines: TGroupBox
        Left = 360
        Top = 7
        Width = 133
        Height = 122
        Caption = 'Special options '
        TabOrder = 3
        object chkSigmaLine: TCheckBox
          Left = 9
          Top = 19
          Width = 82
          Height = 17
          Caption = 'Sigma Lines'
          TabOrder = 0
          OnClick = chkCombiTestClick
        end
        object chkInfo: TCheckBox
          Left = 9
          Top = 43
          Width = 94
          Height = 17
          Caption = 'Show Estimates'
          Checked = True
          State = cbChecked
          TabOrder = 1
          OnClick = chkCombiTestClick
        end
        object chkPoint: TCheckBox
          Left = 10
          Top = 68
          Width = 94
          Height = 17
          Caption = 'Connect Points'
          Checked = True
          State = cbChecked
          TabOrder = 2
          OnClick = chkCombiTestClick
        end
        object ChkTlimit: TCheckBox
          Left = 12
          Top = 94
          Width = 84
          Height = 17
          Hint = 'Use T-based limits'
          Caption = 'Use T-limits'
          TabOrder = 3
          OnClick = chkCheckAllClick
        end
      end
      object GrpFreeze: TGroupBox
        Left = 180
        Top = 137
        Width = 170
        Height = 50
        Caption = 'Freeze estimates'
        TabOrder = 2
        object Label1: TLabel
          Left = 10
          Top = 23
          Width = 74
          Height = 13
          Caption = 'At observation: '
        end
        object edFreeze: TMaskEdit
          Tag = -1
          Left = 96
          Top = 18
          Width = 46
          Height = 21
          AutoSize = False
          EditMask = '99999;1; '
          MaxLength = 5
          TabOrder = 0
          Text = '     '
          OnClick = editClick
          OnEnter = editEnter
        end
      end
      object GrpExclude: TGroupBox
        Left = 360
        Top = 137
        Width = 131
        Height = 50
        Caption = 'Exclude Observation'
        TabOrder = 4
        object EdExp1: TMaskEdit
          Tag = -1
          Left = 6
          Top = 19
          Width = 34
          Height = 21
          AutoSize = False
          EditMask = '99999;1; '
          MaxLength = 5
          TabOrder = 0
          Text = '     '
          OnClick = editClick
          OnEnter = editEnter
        end
        object EdExp2: TMaskEdit
          Tag = -1
          Left = 49
          Top = 19
          Width = 34
          Height = 21
          AutoSize = False
          EditMask = '99999;1; '
          MaxLength = 5
          TabOrder = 1
          Text = '     '
          OnClick = editClick
          OnEnter = editEnter
        end
        object EdExp3: TMaskEdit
          Tag = -1
          Left = 92
          Top = 19
          Width = 34
          Height = 21
          AutoSize = False
          EditMask = '99999;1; '
          MaxLength = 5
          TabOrder = 2
          Text = '     '
          OnClick = editClick
          OnEnter = editEnter
        end
      end
    end
    object tabAxis: TTabSheet
      Caption = 'Graph/Axis'
      ImageIndex = 1
      object Label17: TLabel
        Tag = -1
        Left = 152
        Top = 9
        Width = 29
        Height = 13
        Caption = 'Ticks:'
      end
      object ShowGrpBox: TGroupBox
        Tag = -1
        Left = 7
        Top = 7
        Width = 218
        Height = 90
        Caption = 'Show:'
        TabOrder = 0
        object chkLegend: TCheckBox
          Tag = -1
          Left = 8
          Top = 16
          Width = 56
          Height = 17
          Caption = 'Legend'
          TabOrder = 0
        end
        object chkFrame: TCheckBox
          Tag = -1
          Left = 92
          Top = 16
          Width = 51
          Height = 17
          Caption = 'Frame'
          TabOrder = 3
        end
        object chkGridH: TCheckBox
          Tag = -1
          Left = 92
          Top = 40
          Width = 101
          Height = 16
          Caption = 'Horizontal Grid'
          TabOrder = 4
        end
        object chkGridV: TCheckBox
          Tag = -1
          Left = 92
          Top = 63
          Width = 85
          Height = 17
          Caption = 'Vertical Grid'
          TabOrder = 5
        end
        object chkN: TCheckBox
          Tag = -1
          Left = 8
          Top = 39
          Width = 30
          Height = 17
          Caption = 'N'
          TabOrder = 1
        end
        object ChkYvalue: TCheckBox
          Left = 8
          Top = 62
          Width = 65
          Height = 17
          Caption = 'Y values'
          TabOrder = 2
          OnClick = chkCombiTestClick
        end
      end
      object SizeGrpBox: TGroupBox
        Tag = -1
        Left = 240
        Top = 7
        Width = 137
        Height = 90
        Caption = 'Graph Size:'
        TabOrder = 1
        object Label22: TLabel
          Tag = -1
          Left = 76
          Top = 33
          Width = 28
          Height = 13
          Caption = 'Width'
        end
        object Label23: TLabel
          Tag = -1
          Left = 75
          Top = 64
          Width = 31
          Height = 13
          Caption = 'Height'
        end
        object edSizeX: TMaskEdit
          Tag = -1
          Left = 12
          Top = 28
          Width = 50
          Height = 21
          AutoSize = False
          EditMask = '99999;1; '
          MaxLength = 5
          TabOrder = 0
          Text = '     '
          OnClick = editClick
          OnEnter = editEnter
        end
        object edSizeY: TMaskEdit
          Tag = -1
          Left = 12
          Top = 59
          Width = 50
          Height = 21
          AutoSize = False
          EditMask = '99999;1; '
          MaxLength = 5
          TabOrder = 1
          Text = '     '
          OnClick = editClick
          OnEnter = editEnter
        end
      end
      object AxisGrpBox: TGroupBox
        Tag = -1
        Left = 8
        Top = 101
        Width = 401
        Height = 81
        Caption = 'Axis:'
        TabOrder = 3
        object Label9: TLabel
          Tag = -1
          Left = 16
          Top = 25
          Width = 10
          Height = 13
          Caption = 'X:'
        end
        object Label10: TLabel
          Tag = -1
          Left = 16
          Top = 57
          Width = 10
          Height = 13
          Caption = 'Y:'
        end
        object Label5: TLabel
          Tag = -1
          Left = 237
          Top = 9
          Width = 20
          Height = 13
          Caption = 'Min:'
        end
        object Label6: TLabel
          Tag = -1
          Left = 293
          Top = 9
          Width = 23
          Height = 13
          Caption = 'Max:'
        end
        object Label16: TLabel
          Tag = -1
          Left = 37
          Top = 9
          Width = 25
          Height = 13
          Caption = 'Hide:'
        end
        object Label18: TLabel
          Tag = -1
          Left = 117
          Top = 9
          Width = 21
          Height = 13
          Caption = 'Log:'
        end
        object Label19: TLabel
          Tag = -1
          Left = 77
          Top = 9
          Width = 30
          Height = 13
          Caption = 'Invert:'
        end
        object Label20: TLabel
          Tag = -1
          Left = 344
          Top = 9
          Width = 50
          Height = 13
          Caption = 'Increment:'
        end
        object Label2: TLabel
          Left = 152
          Top = 9
          Width = 29
          Height = 13
          Caption = 'Ticks:'
        end
        object Label3: TLabel
          Left = 192
          Top = 9
          Width = 31
          Height = 13
          Caption = 'Labels'
        end
        object chkHideX: TCheckBox
          Tag = -1
          Left = 37
          Top = 25
          Width = 16
          Height = 16
          TabOrder = 0
        end
        object chkHideY: TCheckBox
          Tag = -1
          Left = 37
          Top = 56
          Width = 16
          Height = 16
          TabOrder = 1
        end
        object chkInvX: TCheckBox
          Tag = -1
          Left = 77
          Top = 25
          Width = 16
          Height = 16
          TabOrder = 2
        end
        object chkLogX: TCheckBox
          Tag = -1
          Left = 117
          Top = 25
          Width = 16
          Height = 16
          TabOrder = 4
        end
        object chkInvY: TCheckBox
          Tag = -1
          Left = 77
          Top = 56
          Width = 16
          Height = 16
          TabOrder = 3
        end
        object chkLogY: TCheckBox
          Tag = -1
          Left = 117
          Top = 56
          Width = 16
          Height = 16
          TabOrder = 5
        end
        object edMinX: TMaskEdit
          Tag = -1
          Left = 237
          Top = 23
          Width = 40
          Height = 21
          AutoSize = False
          EditMask = '99999;1; '
          MaxLength = 5
          ParentShowHint = False
          ShowHint = True
          TabOrder = 10
          Text = '     '
          OnClick = editClick
          OnEnter = editEnter
        end
        object edMaxX: TMaskEdit
          Tag = -1
          Left = 293
          Top = 23
          Width = 40
          Height = 21
          AutoSize = False
          EditMask = '99999;1; '
          MaxLength = 5
          ParentShowHint = False
          ShowHint = True
          TabOrder = 11
          Text = '     '
          OnClick = editClick
          OnEnter = editEnter
        end
        object edMaxY: TMaskEdit
          Tag = -1
          Left = 293
          Top = 54
          Width = 40
          Height = 21
          AutoSize = False
          EditMask = '99999;1; '
          MaxLength = 5
          ParentShowHint = False
          ShowHint = True
          TabOrder = 14
          Text = '     '
          OnClick = editClick
          OnEnter = editEnter
        end
        object edMinY: TMaskEdit
          Tag = -1
          Left = 237
          Top = 54
          Width = 40
          Height = 21
          AutoSize = False
          EditMask = '99999;1; '
          MaxLength = 5
          ParentShowHint = False
          ShowHint = True
          TabOrder = 13
          Text = '     '
          OnClick = editClick
          OnEnter = editEnter
        end
        object edIncX: TMaskEdit
          Tag = -1
          Left = 344
          Top = 23
          Width = 40
          Height = 21
          AutoSize = False
          EditMask = '99999;1; '
          MaxLength = 5
          ParentShowHint = False
          ShowHint = True
          TabOrder = 12
          Text = '     '
          OnClick = editClick
          OnEnter = editEnter
        end
        object edIncY: TMaskEdit
          Tag = -1
          Left = 344
          Top = 53
          Width = 40
          Height = 21
          AutoSize = False
          EditMask = '99999;1; '
          MaxLength = 5
          ParentShowHint = False
          ShowHint = True
          TabOrder = 15
          Text = '     '
          OnClick = editClick
          OnEnter = editEnter
        end
        object chkTickX: TCheckBox
          Tag = -1
          Left = 152
          Top = 25
          Width = 16
          Height = 16
          Hint = 'Show tick marks'
          BiDiMode = bdLeftToRight
          Checked = True
          ParentBiDiMode = False
          ParentShowHint = False
          ShowHint = True
          State = cbChecked
          TabOrder = 6
        end
        object chkTickY: TCheckBox
          Tag = -1
          Left = 152
          Top = 56
          Width = 16
          Height = 16
          Hint = 'Show tick marks'
          Checked = True
          ParentShowHint = False
          ShowHint = True
          State = cbChecked
          TabOrder = 7
        end
        object chkLabelsX: TCheckBox
          Left = 192
          Top = 24
          Width = 17
          Height = 17
          Hint = 'Show tick labels'
          Checked = True
          State = cbChecked
          TabOrder = 8
        end
        object chkLabelsY: TCheckBox
          Left = 192
          Top = 55
          Width = 17
          Height = 17
          Hint = 'Show tick labels'
          Checked = True
          State = cbChecked
          TabOrder = 9
        end
      end
      object chkEdit: TCheckBox
        Tag = -1
        Left = 416
        Top = 108
        Width = 73
        Height = 17
        Caption = 'Edit Graph'
        TabOrder = 4
      end
      object TicksGrpBox: TGroupBox
        Left = 384
        Top = 7
        Width = 113
        Height = 90
        Caption = 'Ticks:'
        TabOrder = 2
        object btnXAngle45: TRadioButton
          Left = 7
          Top = 15
          Width = 42
          Height = 23
          Caption = '45 '
          TabOrder = 0
        end
        object btnXAngle90: TRadioButton
          Left = 59
          Top = 15
          Width = 38
          Height = 23
          Caption = '90'
          TabOrder = 1
        end
        object btnXAngleXA: TRadioButton
          Left = 7
          Top = 39
          Width = 82
          Height = 23
          Caption = 'Alternating'
          TabOrder = 2
        end
        object BtnXAngleDefault: TRadioButton
          Left = 7
          Top = 63
          Width = 62
          Height = 23
          Caption = 'Default'
          Checked = True
          TabOrder = 3
          TabStop = True
        end
      end
    end
    object tabTitle: TTabSheet
      Caption = 'Titles'
      ImageIndex = 3
      object Label27: TLabel
        Left = 8
        Top = 112
        Width = 56
        Height = 13
        Caption = 'X-Axis Text:'
      end
      object Label28: TLabel
        Left = 8
        Top = 152
        Width = 56
        Height = 13
        Caption = 'Y-Axis Text:'
      end
      object Label29: TLabel
        Left = 336
        Top = 172
        Width = 45
        Height = 13
        Caption = 'Font size:'
      end
      object edTitle: TEdit
        Tag = -1
        Left = 8
        Top = 12
        Width = 321
        Height = 21
        TabOrder = 0
        Text = 'Title'
      end
      object edSubtitle: TEdit
        Tag = -1
        Left = 8
        Top = 44
        Width = 321
        Height = 21
        TabOrder = 1
        Text = 'Subtitle'
      end
      object edFoot: TEdit
        Left = 8
        Top = 80
        Width = 169
        Height = 21
        TabOrder = 2
        Text = 'Footnote'
      end
      object TxtGRpBox: TGroupBox
        Tag = -1
        Left = 332
        Top = 7
        Width = 157
        Height = 154
        Hint = 'Note: X and Y in pixels from top left'
        Caption = 'Textbox:'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 7
        object Label12: TLabel
          Tag = -1
          Left = 8
          Top = 14
          Width = 10
          Height = 13
          Caption = 'X:'
        end
        object Label13: TLabel
          Tag = -1
          Left = 40
          Top = 14
          Width = 10
          Height = 13
          Caption = 'Y:'
        end
        object Label14: TLabel
          Tag = -1
          Left = 8
          Top = 54
          Width = 24
          Height = 13
          Caption = 'Text:'
        end
        object edTxtX: TMaskEdit
          Tag = -1
          Left = 8
          Top = 28
          Width = 25
          Height = 21
          EditMask = '999;1; '
          MaxLength = 3
          TabOrder = 0
          Text = '   '
          OnClick = editClick
          OnEnter = editEnter
        end
        object edTxtY: TMaskEdit
          Tag = -1
          Left = 40
          Top = 28
          Width = 29
          Height = 21
          EditMask = '999;1; '
          MaxLength = 3
          TabOrder = 1
          Text = '   '
          OnClick = editClick
          OnEnter = editEnter
        end
        object chkTxtBox: TCheckBox
          Tag = -1
          Left = 80
          Top = 30
          Width = 69
          Height = 17
          Caption = 'Show Box'
          TabOrder = 2
        end
        object mmTxt: TMemo
          Left = 8
          Top = 72
          Width = 137
          Height = 73
          TabOrder = 3
        end
      end
      object edSubFoot: TEdit
        Left = 184
        Top = 80
        Width = 145
        Height = 21
        TabOrder = 3
        Text = 'Subfoot'
      end
      object edXtext: TEdit
        Left = 8
        Top = 128
        Width = 321
        Height = 21
        TabOrder = 4
        Text = 'XText'
      end
      object edYText: TEdit
        Left = 8
        Top = 168
        Width = 321
        Height = 21
        TabOrder = 5
        Text = 'YText'
      end
      object edFontSz: TMaskEdit
        Left = 424
        Top = 168
        Width = 64
        Height = 21
        EditMask = '999;1; '
        MaxLength = 3
        TabOrder = 6
        Text = '   '
        OnClick = editClick
        OnEnter = editEnter
      end
    end
    object tabMisc: TTabSheet
      Caption = 'Misc'
      ImageIndex = 4
      object Label26: TLabel
        Left = 8
        Top = 8
        Width = 61
        Height = 13
        Caption = 'X-Axis Label:'
      end
      object edSaveAs: TEdit
        Tag = -1
        Left = 8
        Top = 171
        Width = 329
        Height = 21
        TabOrder = 1
        Text = 'Save File As'
      end
      object cbXLabel: TComboBox
        Left = 8
        Top = 24
        Width = 145
        Height = 21
        ItemHeight = 13
        TabOrder = 0
        Text = 'XLabel'
      end
    end
  end
  object Panel1: TPanel
    Tag = -1
    Left = 0
    Top = 235
    Width = 509
    Height = 41
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      509
      41)
    object CancelBtn: TButton
      Tag = 3003
      Left = 320
      Top = 13
      Width = 45
      Height = 20
      Anchors = [akLeft, akTop, akBottom]
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 0
    end
    object RunBtn: TButton
      Tag = 3001
      Left = 8
      Top = 13
      Width = 45
      Height = 20
      Anchors = [akLeft, akTop, akBottom]
      Caption = '&Run'
      ModalResult = 1
      TabOrder = 1
      OnClick = RunBtnClick
    end
    object ResetBtn: TButton
      Tag = 3004
      Left = 368
      Top = 13
      Width = 45
      Height = 20
      Anchors = [akLeft, akTop, akBottom]
      Cancel = True
      Caption = 'Rese&t'
      TabOrder = 2
      OnClick = ResetBtnClick
    end
    object PasteBtn: TButton
      Tag = 3005
      Left = 108
      Top = 13
      Width = 45
      Height = 20
      Anchors = [akLeft, akTop, akBottom]
      Caption = '&Paste'
      TabOrder = 3
      OnClick = PasteBtnClick
    end
    object ExecBtn: TButton
      Tag = 3002
      Left = 56
      Top = 13
      Width = 49
      Height = 20
      Anchors = [akLeft, akTop, akBottom]
      Caption = '&Execute'
      TabOrder = 4
      OnClick = ExecBtnClick
    end
  end
end
