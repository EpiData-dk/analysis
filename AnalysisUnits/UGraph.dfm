object GraphForm: TGraphForm
  Left = 567
  Top = 237
  Width = 694
  Height = 496
  Caption = 'GraphForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 686
    Height = 33
    Align = alTop
    Color = clSilver
    TabOrder = 0
    object Edit1: TEdit
      Left = 8
      Top = 4
      Width = 233
      Height = 21
      TabOrder = 0
      Text = 'Write text here - click button'
      OnChange = Edit1Change
    end
    object Button5: TButton
      Tag = 5001
      Left = 248
      Top = 4
      Width = 33
      Height = 25
      Hint = 'Change Text'
      Caption = '&Title'
      TabOrder = 1
      OnClick = Button5Click
    end
    object Button2: TButton
      Tag = 5004
      Left = 392
      Top = 4
      Width = 33
      Height = 25
      Caption = '&Grid'
      TabOrder = 4
      OnClick = Button2Click
    end
    object Button3: TButton
      Tag = 5005
      Left = 424
      Top = 4
      Width = 49
      Height = 25
      Caption = '&Legend'
      TabOrder = 5
      OnClick = Button3Click
    end
    object Button9: TButton
      Tag = 5006
      Left = 472
      Top = 4
      Width = 49
      Height = 25
      Caption = 'F&rame'
      TabOrder = 6
      OnClick = Button9Click
    end
    object Button8: TButton
      Tag = 5002
      Left = 280
      Top = 4
      Width = 49
      Height = 25
      Hint = 'Change Text'
      Caption = 'S&ubtitle'
      TabOrder = 2
      OnClick = Button8Click
    end
    object Button6: TButton
      Tag = 5003
      Left = 328
      Top = 4
      Width = 57
      Height = 25
      Hint = 'Change Text'
      Caption = '&Footnote'
      TabOrder = 3
      OnClick = Button6Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 33
    Width = 686
    Height = 436
    Align = alClient
    TabOrder = 1
    object Panel3: TPanel
      Left = 619
      Top = 1
      Width = 66
      Height = 434
      Align = alRight
      BevelOuter = bvLowered
      TabOrder = 0
      DesignSize = (
        66
        434)
      object Button1: TButton
        Tag = 5007
        Left = 13
        Top = 14
        Width = 45
        Height = 26
        Hint = 'Copy to Clipboard'
        Anchors = [akTop, akRight]
        Caption = '&Copy'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clMenuHighlight
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = Button1Click
      end
      object Button4: TButton
        Tag = 5008
        Left = 13
        Top = 44
        Width = 45
        Height = 26
        Hint = 'Save as Windows Meta File Graph'
        Anchors = [akTop, akRight]
        Caption = '&WMF'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clMenuHighlight
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = Button4Click
      end
      object Button11: TButton
        Tag = 5009
        Left = 13
        Top = 74
        Width = 45
        Height = 26
        Hint = 'Save as Windows Meta File Graph'
        Anchors = [akTop, akRight]
        Caption = '&Png'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clMenuHighlight
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        OnClick = Button11Click
      end
      object Button7: TButton
        Tag = 5010
        Left = 13
        Top = 112
        Width = 45
        Height = 26
        Anchors = [akTop, akRight]
        Caption = 'Edit &All'
        TabOrder = 3
        OnClick = Button7Click
      end
      object Button10: TButton
        Tag = 5011
        Left = 13
        Top = 195
        Width = 45
        Height = 26
        Action = AcClose
        Anchors = [akTop, akRight]
        TabOrder = 4
      end
    end
    object GraphPG: TPageControl
      Left = 1
      Top = 1
      Width = 618
      Height = 434
      Align = alClient
      MultiLine = True
      Style = tsFlatButtons
      TabOrder = 1
      OnChange = GraphPGChange
    end
  end
  object ChartEditor1: TChartEditor
    Left = 361
    Top = 226
  end
  object ActionList1: TActionList
    Left = 280
    Top = 137
    object AcClose: TAction
      Tag = 5011
      Caption = 'E&xit'
      ShortCut = 121
      OnExecute = AcCloseExecute
    end
  end
end
