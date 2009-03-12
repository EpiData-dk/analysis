object TableDlg: TTableDlg
  Left = 556
  Top = 302
  Width = 666
  Height = 349
  BorderIcons = [biSystemMenu]
  Caption = 'Table Dialog'
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
  object CmdPanel: TPanel
    Tag = -1
    Left = 0
    Top = 282
    Width = 658
    Height = 33
    Align = alBottom
    TabOrder = 0
    TabStop = True
    DesignSize = (
      658
      33)
    object MinimizeBtn: TBitBtn
      Tag = 7134
      Left = 171
      Top = 8
      Width = 56
      Height = 20
      Hint = 'Hide options'
      TabOrder = 1
      Visible = False
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
    object ExpandBtn: TBitBtn
      Tag = 7133
      Left = 188
      Top = 8
      Width = 54
      Height = 20
      Hint = 'Show more options'
      Caption = '&More'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      Visible = False
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
    object CancelBtn: TButton
      Tag = 7003
      Left = 102
      Top = 8
      Width = 45
      Height = 20
      Anchors = [akLeft, akTop, akBottom]
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 0
      OnClick = CancelBtnClick
    end
    object RunBtn: TButton
      Tag = 7001
      Left = 5
      Top = 8
      Width = 40
      Height = 20
      Anchors = [akLeft, akTop, akBottom]
      Caption = '&Run'
      ModalResult = 1
      TabOrder = 2
      OnClick = CancelBtnClick
    end
    object ResetBtn: TButton
      Tag = 7004
      Left = 246
      Top = 8
      Width = 41
      Height = 20
      Anchors = [akLeft, akTop, akBottom]
      Cancel = True
      Caption = 'R&eset'
      TabOrder = 5
      OnClick = ResetBtnClick
    end
    object PasteBtn: TButton
      Tag = 7005
      Left = 288
      Top = 8
      Width = 45
      Height = 20
      Anchors = [akLeft, akTop, akBottom]
      Caption = '&Paste'
      ModalResult = 6
      TabOrder = 4
      Visible = False
      OnClick = CancelBtnClick
    end
    object HelpBtn: TButton
      Tag = -1
      Left = 147
      Top = 8
      Width = 20
      Height = 20
      Hint = 'Get Help'
      Caption = '?'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
      OnClick = HelpBtnClick
    end
    object ExecBtn: TButton
      Tag = 7002
      Left = 50
      Top = 8
      Width = 49
      Height = 20
      Anchors = [akLeft, akTop, akBottom]
      Caption = '&Execute'
      TabOrder = 7
      OnClick = ExecBtnClick
    end
  end
  object Panel1: TPanel
    Tag = -1
    Left = 0
    Top = 0
    Width = 658
    Height = 282
    Align = alClient
    Alignment = taLeftJustify
    TabOrder = 1
    TabStop = True
    object FreqPanel: TPanel
      Tag = -1
      Left = 176
      Top = 5
      Width = 217
      Height = 188
      TabOrder = 4
      object GroupBox2: TGroupBox
        Left = 16
        Top = 114
        Width = 177
        Height = 63
        Hint = 'Choose Estimates'
        Caption = 'Other'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        object CIChk: TCheckBox
          Left = 8
          Top = 38
          Width = 121
          Height = 17
          Hint = 'Chi Square Test'
          Alignment = taLeftJustify
          Caption = 'Confidence Interval'
          TabOrder = 0
        end
        object FMisChk: TCheckBox
          Left = 47
          Top = 14
          Width = 82
          Height = 17
          Hint = 'Include Missing Values in Tables'
          Alignment = taLeftJustify
          Caption = 'Show Missing'
          TabOrder = 1
        end
      end
      object GroupBox6: TGroupBox
        Left = 16
        Top = 48
        Width = 97
        Height = 65
        Caption = 'Percentages'
        TabOrder = 1
        object RowFPct: TCheckBox
          Left = 37
          Top = 17
          Width = 44
          Height = 17
          Alignment = taLeftJustify
          Caption = 'Row'
          TabOrder = 1
        end
        object CumPct: TCheckBox
          Left = 4
          Top = 39
          Width = 77
          Height = 17
          Alignment = taLeftJustify
          Caption = 'Cumulative'
          TabOrder = 0
        end
      end
      object AllBtn: TButton
        Tag = 4212
        Left = 12
        Top = 18
        Width = 93
        Height = 17
        Caption = '&All Variables'
        TabOrder = 2
        OnClick = AllBtnClick
      end
    end
    object LabelBox: TGroupBox
      Tag = 7107
      Left = 399
      Top = 218
      Width = 235
      Height = 35
      Caption = 'Labels'
      TabOrder = 1
      object ValueChk: TCheckBox
        Tag = 7131
        Left = 10
        Top = 14
        Width = 103
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Valuelabel+value'
        TabOrder = 0
      end
      object VarLabelChk: TCheckBox
        Tag = 7132
        Left = 126
        Top = 14
        Width = 99
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Variable Names'
        TabOrder = 1
      end
    end
    object FieldsGroup: TGroupBox
      Tag = -1
      Left = 7
      Top = 7
      Width = 162
      Height = 266
      Caption = '&Select Variables'
      TabOrder = 2
      object VarLabel: TLabel
        Tag = -1
        Left = 11
        Top = 238
        Width = 137
        Height = 13
        AutoSize = False
      end
      object DotsLabel: TLabel
        Left = 184
        Top = 181
        Width = 9
        Height = 13
        Caption = '...'
        Visible = False
      end
      object GetVar: TCheckListBox
        Left = 11
        Top = 15
        Width = 142
        Height = 210
        ItemHeight = 13
        TabOrder = 0
        OnClick = GetVarClick
        OnKeyUp = GetVarKeyDown
        OnMouseMove = GetVarMouseMove
      end
    end
    object AdvTablePanel: TPanel
      Tag = -1
      Left = 398
      Top = 10
      Width = 239
      Height = 165
      TabOrder = 5
      OnClick = AdvTablePanelClick
      object EstimatesGrp: TGroupBox
        Tag = 7104
        Left = 5
        Top = 51
        Width = 223
        Height = 66
        Caption = 'Estimation'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnClick = AdvTablePanelClick
        object ARBtn: TRadioButton
          Tag = 7120
          Left = 7
          Top = 19
          Width = 108
          Height = 17
          Hint = 'Outbreak Table'
          Caption = '&Attack Rate + RR'
          TabOrder = 0
          OnClick = AdvTablePanelClick
        end
        object OrBtn: TRadioButton
          Tag = 7122
          Left = 6
          Top = 40
          Width = 79
          Height = 17
          Caption = 'Odds Ratio'
          TabOrder = 1
          OnClick = AdvTablePanelClick
        end
        object RRBtn: TRadioButton
          Tag = 7123
          Left = 85
          Top = 41
          Width = 46
          Height = 17
          Caption = 'RR'
          TabOrder = 2
          OnClick = AdvTablePanelClick
        end
        object ARCIChk: TCheckBox
          Tag = 7121
          Left = 118
          Top = 18
          Width = 55
          Height = 17
          Hint = '95% CI for Attack Rate'
          Alignment = taLeftJustify
          Caption = '(95% C&I)'
          TabOrder = 3
          OnClick = AdvTablePanelClick
        end
        object GammaChk: TCheckBox
          Tag = 7124
          Left = 153
          Top = 41
          Width = 55
          Height = 17
          Hint = 'Gamma Coefficient'
          Alignment = taLeftJustify
          Caption = 'Gamma'
          TabOrder = 4
        end
      end
      object GroupBox1: TGroupBox
        Tag = 7103
        Left = 4
        Top = 5
        Width = 225
        Height = 44
        Caption = 'Table Type'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnClick = AdvTablePanelClick
        object StdBtn: TRadioButton
          Tag = 7117
          Left = 12
          Top = 19
          Width = 67
          Height = 17
          Caption = 'Standar&d'
          Checked = True
          TabOrder = 0
          TabStop = True
          OnClick = AdvTablePanelClick
        end
        object CompactBtn: TRadioButton
          Tag = 7118
          Left = 78
          Top = 20
          Width = 67
          Height = 17
          Hint = 'Condensed table'
          Caption = 'Com&pact'
          TabOrder = 1
          OnClick = AdvTablePanelClick
        end
        object FVbtn: TRadioButton
          Tag = 7119
          Left = 149
          Top = 20
          Width = 67
          Height = 17
          Hint = 'First variables against all other variables'
          Caption = '&First x All'
          TabOrder = 2
          OnClick = AdvTablePanelClick
        end
      end
      object StatsGrp: TGroupBox
        Tag = 7105
        Left = 7
        Top = 118
        Width = 220
        Height = 39
        Hint = 'Choose Estimates'
        Caption = 'Testing '
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        OnClick = AdvTablePanelClick
        OnDblClick = AdvTablePanelClick
        object ChiSQChk: TCheckBox
          Tag = 7125
          Left = 6
          Top = 14
          Width = 52
          Height = 20
          Hint = 'Chi Square test'
          Alignment = taLeftJustify
          Caption = 'Chi Sq'
          TabOrder = 0
        end
        object ExactChk: TCheckBox
          Tag = 7126
          Left = 75
          Top = 16
          Width = 55
          Height = 17
          Hint = 'Chi Square Test'
          Alignment = taLeftJustify
          Caption = 'Exact p'
          TabOrder = 1
        end
      end
    end
    object TablePanel: TPanel
      Tag = -1
      Left = 173
      Top = 9
      Width = 217
      Height = 185
      TabOrder = 3
      object ColumnVar: TLabel
        Tag = -1
        Left = 54
        Top = 16
        Width = 3
        Height = 16
        Hint = 'Column Variable (Outcome)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object RowVar: TLabel
        Tag = -1
        Left = 38
        Top = 38
        Width = 3
        Height = 16
        Hint = 'Row Variable (exposure...)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Variables: TLabel
        Tag = -1
        Left = 13
        Top = 61
        Width = 3
        Height = 16
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label2: TLabel
        Tag = 7109
        Left = 11
        Top = 38
        Width = 22
        Height = 13
        Caption = 'Row'
      end
      object Label1: TLabel
        Tag = 7108
        Left = 11
        Top = 16
        Width = 35
        Height = 13
        Caption = 'Column'
      end
      object PercentageGrp: TGroupBox
        Tag = 7101
        Left = 7
        Top = 113
        Width = 204
        Height = 66
        Hint = 'Add Percentages'
        Caption = 'Percentages'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        object RowPct: TCheckBox
          Tag = 7111
          Left = 8
          Top = 11
          Width = 49
          Height = 25
          Alignment = taLeftJustify
          Caption = '&Row'
          TabOrder = 0
        end
        object ColumnPct: TCheckBox
          Tag = 7110
          Left = 68
          Top = 16
          Width = 54
          Height = 17
          Alignment = taLeftJustify
          Caption = '&Column'
          TabOrder = 1
        end
        object TotalPct: TCheckBox
          Tag = 7112
          Left = 143
          Top = 16
          Width = 42
          Height = 17
          Alignment = taLeftJustify
          Caption = 'T&otal'
          TabOrder = 2
        end
        object MissingChk: TCheckBox
          Tag = 7113
          Left = 40
          Top = 41
          Width = 82
          Height = 17
          Hint = 'Include Missing Values in Tables'
          Alignment = taLeftJustify
          Caption = 'Sho&w Missing'
          TabOrder = 3
        end
      end
    end
    object GroupBox3: TGroupBox
      Tag = 7106
      Left = 397
      Top = 177
      Width = 237
      Height = 41
      Hint = 'Change output color, shades etc.'
      Caption = 'Output design'
      TabOrder = 6
      object LineClass: TRadioButton
        Tag = 7127
        Left = 5
        Top = 16
        Width = 49
        Height = 17
        Caption = 'Line'
        TabOrder = 0
      end
      object BoxClass: TRadioButton
        Tag = 7128
        Left = 58
        Top = 16
        Width = 49
        Height = 17
        Caption = 'Box'
        TabOrder = 1
      end
      object FilledClass: TRadioButton
        Tag = 7129
        Left = 114
        Top = 16
        Width = 49
        Height = 17
        Caption = 'Filled'
        TabOrder = 2
      end
      object ShadedClass: TRadioButton
        Tag = 7130
        Left = 175
        Top = 16
        Width = 49
        Height = 17
        Caption = 'Shaded'
        TabOrder = 3
      end
    end
    object TblShowGrp: TGroupBox
      Tag = 7102
      Left = 167
      Top = 195
      Width = 227
      Height = 59
      Hint = 'Default sorting: See helpfile (F1) '
      Caption = 'Sorting '
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
      object Label9: TLabel
        Tag = 7116
        Left = 10
        Top = 36
        Width = 30
        Height = 13
        Caption = 'Total: '
      end
      object SortSA: TRadioButton
        Tag = 7114
        Left = 42
        Top = 15
        Width = 77
        Height = 17
        Hint = 'Row and Column category'
        Caption = 'Ascending'
        TabOrder = 0
      end
      object SortSAT: TRadioButton
        Tag = 7114
        Left = 40
        Top = 35
        Width = 80
        Height = 17
        Hint = 'On total Count'
        Caption = 'Ascending'
        TabOrder = 1
      end
      object SortSDT: TRadioButton
        Tag = 7115
        Left = 131
        Top = 35
        Width = 79
        Height = 17
        Hint = 'On total Count'
        Caption = 'Descending'
        TabOrder = 2
      end
      object SortSD: TRadioButton
        Tag = 7115
        Left = 130
        Top = 16
        Width = 82
        Height = 17
        Hint = 'Row and Column category'
        Caption = 'Descending'
        TabOrder = 3
      end
    end
    object HelpPanel: TPanel
      Tag = -1
      Left = 32
      Top = 64
      Width = 241
      Height = 113
      Hint = 'Short Instruction'
      BevelInner = bvLowered
      BevelWidth = 2
      BorderWidth = 1
      BorderStyle = bsSingle
      Color = clCaptionText
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMenuHighlight
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      object Label3: TLabel
        Left = 12
        Top = 13
        Width = 32
        Height = 13
        Caption = 'Label3'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clHighlight
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label4: TLabel
        Left = 12
        Top = 29
        Width = 32
        Height = 13
        Caption = 'Label3'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clMenuHighlight
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label5: TLabel
        Left = 12
        Top = 45
        Width = 32
        Height = 13
        Caption = 'Label3'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clMenuHighlight
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label6: TLabel
        Left = 12
        Top = 61
        Width = 32
        Height = 13
        Caption = 'Label3'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clHighlight
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label7: TLabel
        Left = 12
        Top = 77
        Width = 32
        Height = 13
        Caption = 'Label3'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clHighlight
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object CloseHelpBtn: TButton
        Tag = 7006
        Left = 204
        Top = 7
        Width = 27
        Height = 16
        Caption = 'OK'
        TabOrder = 0
        OnClick = CloseHelpBtnClick
      end
    end
  end
end
