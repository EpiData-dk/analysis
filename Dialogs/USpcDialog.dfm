object frmSPCDialog: TfrmSPCDialog
  Left = 544
  Top = 246
  Width = 478
  Height = 323
  BorderIcons = [biSystemMenu]
  Caption = 'frmSPCDialog'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 470
    Height = 255
    ActivePage = tabVariables
    Align = alClient
    TabOrder = 0
    object tabVariables: TTabSheet
      Caption = 'Variables'
      object Label1: TLabel
        Left = 168
        Top = 24
        Width = 6
        Height = 13
        Caption = '_'
      end
      object Label2: TLabel
        Left = 168
        Top = 56
        Width = 6
        Height = 13
        Caption = '_'
      end
      object Label3: TLabel
        Left = 168
        Top = 88
        Width = 6
        Height = 13
        Caption = '_'
      end
      object ComboBox1: TComboBox
        Left = 8
        Top = 16
        Width = 145
        Height = 21
        ItemHeight = 13
        TabOrder = 0
        Text = 'ComboBox1'
      end
      object ComboBox2: TComboBox
        Left = 8
        Top = 48
        Width = 145
        Height = 21
        ItemHeight = 13
        TabOrder = 1
        Text = 'ComboBox2'
      end
      object ComboBox3: TComboBox
        Left = 8
        Top = 80
        Width = 145
        Height = 21
        ItemHeight = 13
        TabOrder = 2
        Text = 'ComboBox3'
      end
      object EditChk: TCheckBox
        Tag = -1
        Left = 344
        Top = 175
        Width = 41
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Edit:'
        TabOrder = 3
      end
      object ComboBox4: TComboBox
        Left = 8
        Top = 112
        Width = 145
        Height = 21
        ItemHeight = 13
        TabOrder = 4
        Text = 'ComboBox4'
      end
      object ComboBox5: TComboBox
        Left = 8
        Top = 184
        Width = 145
        Height = 21
        ItemHeight = 13
        TabOrder = 5
        Text = 'ComboBox5'
      end
    end
    object tabSPC: TTabSheet
      Caption = 'SPC'
      ImageIndex = 2
      object GroupBox2: TGroupBox
        Left = 8
        Top = 8
        Width = 185
        Height = 185
        Caption = 'Tests:'
        TabOrder = 0
        object cbTest1: TCheckBox
          Left = 16
          Top = 64
          Width = 97
          Height = 17
          Caption = 'Test 1'
          TabOrder = 0
          OnClick = cbTest1Click
        end
        object cbTest2: TCheckBox
          Left = 16
          Top = 88
          Width = 97
          Height = 17
          Caption = 'Test 2'
          TabOrder = 1
          OnClick = cbTest1Click
        end
        object cbTest3: TCheckBox
          Left = 16
          Top = 112
          Width = 97
          Height = 17
          Caption = 'Test 3'
          TabOrder = 2
          OnClick = cbTest1Click
        end
        object cbCheckAll: TCheckBox
          Left = 16
          Top = 24
          Width = 97
          Height = 17
          Caption = 'Check All'
          TabOrder = 3
          OnClick = cbCheckAllClick
        end
        object cbTest4: TCheckBox
          Left = 16
          Top = 136
          Width = 97
          Height = 17
          Caption = 'Test 4'
          TabOrder = 4
          OnClick = cbTest1Click
        end
        object cbTest5: TCheckBox
          Left = 16
          Top = 160
          Width = 97
          Height = 17
          Caption = 'Test 5'
          TabOrder = 5
          OnClick = cbTest1Click
        end
      end
      object GroupBox3: TGroupBox
        Left = 200
        Top = 8
        Width = 185
        Height = 185
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
          Width = 9
          Height = 13
          Caption = '3:'
        end
        object edBreak1: TMaskEdit
          Left = 32
          Top = 24
          Width = 118
          Height = 21
          EditMask = '99999999;1; '
          MaxLength = 8
          TabOrder = 0
          Text = '        '
        end
        object MaskEdit1: TMaskEdit
          Left = 32
          Top = 56
          Width = 118
          Height = 21
          EditMask = '99999999;1; '
          MaxLength = 8
          TabOrder = 1
          Text = '        '
        end
        object MaskEdit2: TMaskEdit
          Left = 32
          Top = 88
          Width = 118
          Height = 21
          EditMask = '99999999;1; '
          MaxLength = 8
          TabOrder = 2
          Text = '        '
        end
      end
    end
    object tabGraph: TTabSheet
      Caption = 'Graph'
      ImageIndex = 1
      object GenGrpBox: TGroupBox
        Tag = -1
        Left = 7
        Top = 7
        Width = 190
        Height = 63
        Caption = 'Show'
        TabOrder = 0
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
      object GroupBox1: TGroupBox
        Tag = -1
        Left = 201
        Top = 7
        Width = 120
        Height = 63
        Caption = 'Graph Size'
        TabOrder = 1
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
      object AxisGrpBox: TGroupBox
        Tag = -1
        Left = 8
        Top = 77
        Width = 313
        Height = 81
        Caption = 'Axis:'
        TabOrder = 2
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
      object TxtGRpBox: TGroupBox
        Tag = -1
        Left = 8
        Top = 165
        Width = 313
        Height = 60
        Hint = 'Note: X and Y in pixels from top left'
        Caption = 'Textbox:'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
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
    end
    object tabTitle: TTabSheet
      Caption = 'Titles'
      ImageIndex = 3
      object Edit1: TEdit
        Tag = -1
        Left = 8
        Top = 12
        Width = 377
        Height = 21
        TabOrder = 0
        Text = 'Title'
      end
      object Edit2: TEdit
        Tag = -1
        Left = 8
        Top = 44
        Width = 377
        Height = 21
        TabOrder = 1
        Text = 'Subtitle'
      end
      object Edit6: TEdit
        Tag = -1
        Left = 8
        Top = 187
        Width = 329
        Height = 21
        TabOrder = 2
        Text = 'Save File As'
      end
    end
  end
  object Panel1: TPanel
    Tag = -1
    Left = 0
    Top = 255
    Width = 470
    Height = 41
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      470
      41)
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
    end
    object ResetBtn: TButton
      Tag = 3004
      Left = 296
      Top = 13
      Width = 45
      Height = 20
      Anchors = [akLeft, akTop, akBottom]
      Cancel = True
      Caption = 'R&eset'
      TabOrder = 2
    end
    object PasteBtn: TButton
      Tag = 3005
      Left = 344
      Top = 13
      Width = 45
      Height = 20
      Anchors = [akLeft, akTop, akBottom]
      Caption = '&Paste'
      ModalResult = 6
      TabOrder = 3
      Visible = False
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
    end
  end
end
