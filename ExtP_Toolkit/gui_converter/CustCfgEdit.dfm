object CustConfigEditor: TCustConfigEditor
  Left = 192
  Top = 114
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Edit Custom Config File'
  ClientHeight = 499
  ClientWidth = 570
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 16
  object SaveBtn: TButton
    Left = 344
    Top = 408
    Width = 81
    Height = 30
    Hint = 'Save custom config file'
    Caption = 'Save'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    OnClick = SaveBtnClick
  end
  object CloseBtn: TButton
    Left = 448
    Top = 408
    Width = 81
    Height = 30
    Hint = 'Close custom config file editor'
    Caption = 'Close'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    OnClick = CloseBtnClick
  end
  object UnknownGroupBox: TGroupBox
    Left = 16
    Top = 16
    Width = 281
    Height = 465
    Caption = ' Unmapped Component Classes '
    TabOrder = 0
    object UnknownListBox: TListBox
      Left = 8
      Top = 28
      Width = 265
      Height = 301
      ItemHeight = 16
      TabOrder = 0
      OnClick = UnknownListBoxClick
    end
    object UnknownHelpMemo: TMemo
      Left = 8
      Top = 336
      Width = 265
      Height = 121
      TabStop = False
      BorderStyle = bsNone
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      Lines.Strings = (
        '(*) indicates unmapped classes from your forms.  If '
        'a class is descended from or is similar to a standard '
        'VCL/LCL class, select the standard class to map to.  '
        'If you don'#39't need to convert a class'#39's objects, leave '
        'it unmapped.'
        ''
        'If a class is an inherited form, select the ancestor '
        'form'#39's unit instead.')
      ParentFont = False
      ReadOnly = True
      TabOrder = 1
    end
  end
  object MappingGroupBox: TGroupBox
    Left = 312
    Top = 16
    Width = 241
    Height = 345
    Caption = ' Component Mapping '
    TabOrder = 1
    object MapToLbl: TLabel
      Left = 8
      Top = 25
      Width = 225
      Height = 16
      Alignment = taCenter
      AutoSize = False
      Caption = 'Map class to this'
    end
    object FormUnitLbl: TLabel
      Left = 8
      Top = 89
      Width = 225
      Height = 16
      Alignment = taCenter
      AutoSize = False
      Caption = 'Ancestor form'#39's unit'
    end
    object PropsLbl: TLabel
      Left = 8
      Top = 153
      Width = 225
      Height = 16
      Alignment = taCenter
      AutoSize = False
      Caption = 'Other properties to map'
    end
    object PropsHelpLbl: TLabel
      Left = 8
      Top = 169
      Width = 225
      Height = 16
      Alignment = taCenter
      AutoSize = False
      Caption = 'Enter mappings like this:  vclprop=extprop'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object MapToComboBox: TComboBox
      Left = 8
      Top = 49
      Width = 225
      Height = 24
      Style = csDropDownList
      ItemHeight = 16
      TabOrder = 0
      OnChange = MapToComboBoxChange
    end
    object FormUnitComboBox: TComboBox
      Left = 8
      Top = 113
      Width = 225
      Height = 24
      Style = csDropDownList
      ItemHeight = 16
      TabOrder = 1
      OnChange = FormUnitComboBoxChange
    end
    object PropsMemo: TMemo
      Left = 8
      Top = 192
      Width = 225
      Height = 137
      ScrollBars = ssBoth
      TabOrder = 2
      WordWrap = False
    end
  end
end
