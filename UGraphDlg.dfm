object GraphDlg: TGraphDlg
  Left = 415
  Top = 247
  Width = 718
  Height = 323
  BorderIcons = [biSystemMenu]
  Caption = 'h'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label8: TLabel
    Tag = -1
    Left = 200
    Top = 17
    Width = 20
    Height = 13
    Caption = 'Min:'
  end
  object Label11: TLabel
    Tag = -1
    Left = 200
    Top = 41
    Width = 23
    Height = 13
    Caption = 'Max:'
  end
  object CmdPanel: TPanel
    Tag = -1
    Left = 0
    Top = 247
    Width = 710
    Height = 42
    Align = alBottom
    TabOrder = 0
    TabStop = True
    DesignSize = (
      710
      42)
    object MinimizeBtn: TBitBtn
      Tag = 8002
      Left = 168
      Top = 13
      Width = 57
      Height = 20
      Caption = 'Less'
      TabOrder = 2
      OnClick = ExpandBtnClick
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        333333333333333333FF3333333333333744333333333333F773333333333337
        44473333333333F777F3333333333744444333333333F7733733333333374444
        4433333333F77333733333333744444447333333F7733337F333333744444444
        433333F77333333733333744444444443333377FFFFFFF7FFFFF999999999999
        9999733777777777777333CCCCCCCCCC33333773FF333373F3333333CCCCCCCC
        C333333773FF3337F333333333CCCCCCC33333333773FF373F3333333333CCCC
        CC333333333773FF73F33333333333CCCCC3333333333773F7F3333333333333
        CCC333333333333777FF33333333333333CC3333333333333773}
      NumGlyphs = 2
    end
    object CancelBtn: TButton
      Tag = 3003
      Left = 104
      Top = 13
      Width = 45
      Height = 20
      Anchors = [akLeft, akTop, akBottom]
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
      OnClick = CancelBtnClick
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
      TabOrder = 0
      OnClick = CancelBtnClick
    end
    object ResetBtn: TButton
      Tag = 3004
      Left = 232
      Top = 13
      Width = 45
      Height = 20
      Anchors = [akLeft, akTop, akBottom]
      Cancel = True
      Caption = 'R&eset'
      TabOrder = 5
      OnClick = ResetBtnClick
    end
    object PasteBtn: TButton
      Tag = 3005
      Left = 280
      Top = 13
      Width = 45
      Height = 20
      Anchors = [akLeft, akTop, akBottom]
      Caption = '&Paste'
      ModalResult = 6
      TabOrder = 4
      Visible = False
      OnClick = CancelBtnClick
    end
    object ExpandBtn: TBitBtn
      Tag = 8001
      Left = 168
      Top = 13
      Width = 57
      Height = 20
      Caption = 'More'
      TabOrder = 3
      OnClick = ExpandBtnClick
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        33333FF3333333333333447333333333333377FFF33333333333744473333333
        333337773FF3333333333444447333333333373F773FF3333333334444447333
        33333373F3773FF3333333744444447333333337F333773FF333333444444444
        733333373F3333773FF333334444444444733FFF7FFFFFFF77FF999999999999
        999977777777777733773333CCCCCCCCCC3333337333333F7733333CCCCCCCCC
        33333337F3333F773333333CCCCCCC3333333337333F7733333333CCCCCC3333
        333333733F77333333333CCCCC333333333337FF7733333333333CCC33333333
        33333777333333333333CC333333333333337733333333333333}
      NumGlyphs = 2
    end
    object ExecBtn: TButton
      Tag = 3002
      Left = 56
      Top = 13
      Width = 49
      Height = 20
      Anchors = [akLeft, akTop, akBottom]
      Caption = '&Execute'
      TabOrder = 6
      OnClick = ExecBtnClick
    end
    object SpcGrpBox: TPanel
      Tag = -1
      Left = 400
      Top = 4
      Width = 297
      Height = 33
      BorderWidth = 1
      BorderStyle = bsSingle
      Caption = 'SpcGrpBox'
      TabOrder = 7
      object Label7: TLabel
        Tag = -1
        Left = 76
        Top = 8
        Width = 36
        Height = 13
        Caption = 'Breaks:'
      end
      object Label21: TLabel
        Tag = -1
        Left = 9
        Top = 8
        Width = 21
        Height = 13
        Caption = 'Test'
      end
      object Break1: TMaskEdit
        Tag = -1
        Left = 120
        Top = 4
        Width = 73
        Height = 21
        TabOrder = 0
      end
      object Break2: TMaskEdit
        Tag = -1
        Left = 208
        Top = 4
        Width = 72
        Height = 21
        TabOrder = 1
      end
      object SPCTest: TCheckBox
        Tag = -1
        Left = 39
        Top = 6
        Width = 14
        Height = 17
        Alignment = taLeftJustify
        Caption = 'spctest'
        Checked = True
        State = cbChecked
        TabOrder = 2
      end
    end
  end
  object Panel1: TPanel
    Tag = -1
    Left = 0
    Top = 0
    Width = 710
    Height = 247
    Align = alClient
    TabOrder = 1
    TabStop = True
    object Label1: TLabel
      Tag = -1
      Left = 176
      Top = 16
      Width = 6
      Height = 13
      Caption = '_'
      Transparent = False
    end
    object Label2: TLabel
      Tag = -1
      Left = 176
      Top = 42
      Width = 6
      Height = 13
      Caption = '_'
    end
    object Label3: TLabel
      Tag = -1
      Left = 176
      Top = 68
      Width = 6
      Height = 13
      Caption = '_'
    end
    object Label4: TLabel
      Tag = -1
      Left = 176
      Top = 94
      Width = 6
      Height = 13
      Caption = '_'
    end
    object Label24: TLabel
      Tag = -1
      Left = 176
      Top = 118
      Width = 6
      Height = 13
      Caption = '_'
    end
    object ComboBox1: TComboBox
      Tag = -1
      Left = 10
      Top = 9
      Width = 150
      Height = 21
      ItemHeight = 13
      TabOrder = 0
      Text = 'Choose X Variable'
      OnSelect = ComboBoxSelect
    end
    object ComboBox2: TComboBox
      Tag = -1
      Left = 10
      Top = 35
      Width = 150
      Height = 21
      ItemHeight = 13
      TabOrder = 1
      Text = 'Choose Y variable'
      OnSelect = ComboBoxSelect
    end
    object ComboBox3: TComboBox
      Tag = -1
      Left = 10
      Top = 61
      Width = 150
      Height = 21
      Hint = 'Check "BY" for use as group variable'
      ItemHeight = 13
      TabOrder = 2
      Text = 'Y Variable (optional)'
      OnSelect = ComboBoxSelect
    end
    object ComboBox4: TComboBox
      Tag = -1
      Left = 10
      Top = 88
      Width = 150
      Height = 21
      ItemHeight = 13
      TabOrder = 3
      Text = 'Y Variable (optional)'
      OnSelect = ComboBoxSelect
    end
    object Edit1: TEdit
      Tag = -1
      Left = 8
      Top = 172
      Width = 377
      Height = 21
      TabOrder = 4
      Text = 'Title'
    end
    object Edit2: TEdit
      Tag = -1
      Left = 8
      Top = 196
      Width = 377
      Height = 21
      TabOrder = 5
      Text = 'Subtitle'
    end
    object Edit6: TEdit
      Tag = -1
      Left = 8
      Top = 227
      Width = 329
      Height = 21
      TabOrder = 6
      Text = 'Save File As'
    end
    object TxtGRpBox: TGroupBox
      Tag = -1
      Left = 396
      Top = 149
      Width = 309
      Height = 60
      Hint = 'Note: X and Y in pixels from top left'
      Caption = 'Textbox:'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
      object Label12: TLabel
        Tag = -1
        Left = 24
        Top = 14
        Width = 10
        Height = 13
        Caption = 'X:'
      end
      object Label13: TLabel
        Tag = -1
        Left = 64
        Top = 14
        Width = 10
        Height = 13
        Caption = 'Y:'
      end
      object Label14: TLabel
        Tag = -1
        Left = 96
        Top = 14
        Width = 24
        Height = 13
        Caption = 'Text:'
      end
      object Label15: TLabel
        Tag = -1
        Left = 282
        Top = 14
        Width = 21
        Height = 13
        Caption = 'Box:'
      end
      object xpos1: TMaskEdit
        Tag = -1
        Left = 16
        Top = 28
        Width = 26
        Height = 21
        TabOrder = 0
      end
      object ypos1: TMaskEdit
        Tag = -1
        Left = 56
        Top = 28
        Width = 30
        Height = 21
        TabOrder = 1
      end
      object Edit3: TEdit
        Tag = -1
        Left = 96
        Top = 28
        Width = 177
        Height = 21
        TabOrder = 2
      end
      object btyp1: TCheckBox
        Tag = -1
        Left = 284
        Top = 32
        Width = 17
        Height = 17
        TabOrder = 3
      end
    end
    object AxisGrpBox: TGroupBox
      Tag = -1
      Left = 396
      Top = 69
      Width = 309
      Height = 81
      Caption = 'Axis:'
      TabOrder = 8
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
        Left = 173
        Top = 9
        Width = 20
        Height = 13
        Caption = 'Min:'
      end
      object Label6: TLabel
        Tag = -1
        Left = 221
        Top = 9
        Width = 23
        Height = 13
        Caption = 'Max:'
      end
      object Label16: TLabel
        Tag = -1
        Left = 37
        Top = 9
        Width = 27
        Height = 13
        Caption = 'Show'
      end
      object Label17: TLabel
        Tag = -1
        Left = 141
        Top = 9
        Width = 21
        Height = 13
        Caption = 'Tick'
      end
      object Label18: TLabel
        Tag = -1
        Left = 109
        Top = 9
        Width = 18
        Height = 13
        Caption = 'Log'
      end
      object Label19: TLabel
        Tag = -1
        Left = 69
        Top = 9
        Width = 27
        Height = 13
        Caption = 'Invert'
      end
      object Label20: TLabel
        Tag = -1
        Left = 256
        Top = 9
        Width = 50
        Height = 13
        Caption = 'Increment:'
      end
      object XHideChk: TCheckBox
        Tag = -1
        Left = 40
        Top = 25
        Width = 17
        Height = 17
        Alignment = taLeftJustify
        TabOrder = 5
      end
      object YHideChk: TCheckBox
        Tag = -1
        Left = 45
        Top = 56
        Width = 13
        Height = 17
        Alignment = taLeftJustify
        TabOrder = 2
      end
      object XInvChk: TCheckBox
        Tag = -1
        Left = 80
        Top = 25
        Width = 13
        Height = 17
        Alignment = taLeftJustify
        TabOrder = 4
      end
      object XLogChk: TCheckBox
        Tag = -1
        Left = 112
        Top = 25
        Width = 12
        Height = 17
        Alignment = taLeftJustify
        TabOrder = 3
      end
      object YInvChk: TCheckBox
        Tag = -1
        Left = 78
        Top = 56
        Width = 15
        Height = 17
        Alignment = taLeftJustify
        TabOrder = 1
      end
      object YLogChk: TCheckBox
        Tag = -1
        Left = 111
        Top = 56
        Width = 14
        Height = 17
        Alignment = taLeftJustify
        TabOrder = 0
      end
      object xtickchk: TCheckBox
        Tag = -1
        Left = 142
        Top = 25
        Width = 17
        Height = 17
        Hint = 'Add tick marks'
        Alignment = taLeftJustify
        Checked = True
        ParentShowHint = False
        ShowHint = True
        State = cbChecked
        TabOrder = 6
      end
      object YtickChk: TCheckBox
        Tag = -1
        Left = 146
        Top = 56
        Width = 14
        Height = 18
        Hint = 'Add tick marks'
        Alignment = taLeftJustify
        Checked = True
        ParentShowHint = False
        ShowHint = True
        State = cbChecked
        TabOrder = 7
      end
      object xmin: TMaskEdit
        Tag = -1
        Left = 168
        Top = 23
        Width = 41
        Height = 21
        Hint = 'Not yet implemented!'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 8
      end
      object xmax: TMaskEdit
        Tag = -1
        Left = 216
        Top = 23
        Width = 41
        Height = 21
        Hint = 'Not yet implemented!'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 9
      end
      object ymax: TMaskEdit
        Tag = -1
        Left = 216
        Top = 54
        Width = 41
        Height = 21
        Hint = 'Not yet implemented!'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 10
      end
      object ymin: TMaskEdit
        Tag = -1
        Left = 168
        Top = 54
        Width = 41
        Height = 21
        Hint = 'Not yet implemented!'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 11
      end
      object xinc: TMaskEdit
        Tag = -1
        Left = 264
        Top = 23
        Width = 41
        Height = 21
        Hint = 'Not yet implemented!'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 12
      end
      object YINC: TMaskEdit
        Tag = -1
        Left = 263
        Top = 53
        Width = 41
        Height = 21
        Hint = 'Not yet implemented!'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 13
      end
    end
    object GenGrpBox: TGroupBox
      Tag = -1
      Left = 396
      Top = 7
      Width = 190
      Height = 63
      Caption = 'Show'
      TabOrder = 9
      object LegendChk: TCheckBox
        Tag = -1
        Left = 8
        Top = 16
        Width = 56
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Legend:'
        TabOrder = 2
      end
      object FrameChk: TCheckBox
        Tag = -1
        Left = 83
        Top = 16
        Width = 51
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Frame:'
        TabOrder = 3
      end
      object HGridChk: TCheckBox
        Tag = -1
        Left = 8
        Top = 40
        Width = 92
        Height = 16
        Alignment = taLeftJustify
        Caption = 'Grid: Horizontial'
        TabOrder = 0
      end
      object VGridChk: TCheckBox
        Tag = -1
        Left = 106
        Top = 39
        Width = 57
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Vertical:'
        TabOrder = 1
      end
      object NChk: TCheckBox
        Tag = -1
        Left = 154
        Top = 16
        Width = 30
        Height = 17
        Alignment = taLeftJustify
        Caption = 'N'
        TabOrder = 4
      end
    end
    object EditChk: TCheckBox
      Tag = -1
      Left = 344
      Top = 231
      Width = 41
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Edit:'
      TabOrder = 10
    end
    object ComboBox5: TComboBox
      Tag = -1
      Left = 10
      Top = 113
      Width = 150
      Height = 21
      ItemHeight = 13
      TabOrder = 11
      Text = 'By Variable (optional)'
      OnSelect = ComboBoxSelect
    end
    object GroupBox1: TGroupBox
      Tag = -1
      Left = 592
      Top = 7
      Width = 108
      Height = 63
      Caption = 'Graph Size'
      TabOrder = 12
      object Label22: TLabel
        Tag = -1
        Left = 12
        Top = 17
        Width = 33
        Height = 13
        Caption = 'Length'
      end
      object Label23: TLabel
        Tag = -1
        Left = 11
        Top = 43
        Width = 31
        Height = 13
        Caption = 'Height'
      end
      object SizeX: TMaskEdit
        Tag = -1
        Left = 60
        Top = 12
        Width = 40
        Height = 21
        TabOrder = 0
      end
      object SizeY: TMaskEdit
        Tag = -1
        Left = 60
        Top = 35
        Width = 39
        Height = 21
        TabOrder = 1
      end
    end
    object WeigthBox: TGroupBox
      Tag = -1
      Left = 396
      Top = 210
      Width = 309
      Height = 41
      Caption = 'Weigth'
      TabOrder = 13
      object ComboBox6: TComboBox
        Tag = -1
        Left = 72
        Top = 12
        Width = 153
        Height = 21
        ItemHeight = 13
        TabOrder = 0
        Text = 'Weigth variable (optional)'
      end
    end
  end
end
