object FrmBigSearchOptions: TFrmBigSearchOptions
  Left = 572
  Top = 312
  Caption = 'Find text in dfm files'
  ClientHeight = 258
  ClientWidth = 384
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
  object lblTextToFind: TLabel
    Left = 8
    Top = 12
    Width = 56
    Height = 13
    Caption = '&Text to find:'
    FocusControl = cmbTextToFind
  end
  object cmbTextToFind: TComboBox
    Left = 79
    Top = 8
    Width = 297
    Height = 21
    ItemHeight = 13
    TabOrder = 0
  end
  object gbxWhere: TGroupBox
    Left = 8
    Top = 40
    Width = 185
    Height = 95
    Caption = 'Where'
    TabOrder = 1
    object rbSearchAllFilesInProject: TRadioButton
      Left = 8
      Top = 16
      Width = 153
      Height = 17
      Caption = 'Search all files in &project'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object rbSearchAllOpenFiles: TRadioButton
      Left = 8
      Top = 36
      Width = 153
      Height = 17
      Caption = 'Search all &open files'
      TabOrder = 1
    end
    object rbSearchInDirectories: TRadioButton
      Left = 8
      Top = 56
      Width = 161
      Height = 17
      Caption = 'Search in &directories'
      TabOrder = 2
    end
  end
  object btnOK: TButton
    Left = 141
    Top = 224
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 221
    Top = 224
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object btnHelp: TButton
    Left = 301
    Top = 224
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 5
    OnClick = btnHelpClick
  end
  object gbxSearchDirectoryOptions: TGroupBox
    Left = 7
    Top = 141
    Width = 369
    Height = 65
    Caption = 'Search Directory Options'
    TabOrder = 6
    object lblFileMask: TLabel
      Left = 8
      Top = 24
      Width = 47
      Height = 13
      Caption = 'File &mask:'
    end
    object cmbFileMask: TComboBox
      Left = 64
      Top = 16
      Width = 209
      Height = 21
      ItemHeight = 13
      TabOrder = 0
      OnChange = cmbFileMaskChange
    end
    object btnBrowse: TButton
      Left = 280
      Top = 16
      Width = 75
      Height = 23
      Caption = '&Browse'
      TabOrder = 1
      OnClick = btnBrowseClick
    end
    object chbSearchRecursive: TCheckBox
      Left = 64
      Top = 40
      Width = 161
      Height = 17
      Caption = 'Include &subdirectories'
      TabOrder = 2
    end
  end
  object gbxOptions: TGroupBox
    Left = 199
    Top = 40
    Width = 178
    Height = 95
    Caption = 'Options'
    TabOrder = 2
    object chbWildCardSearch: TCheckBox
      Left = 12
      Top = 16
      Width = 153
      Height = 17
      Hint = 'Allows searching for wildcards (* and ?)'
      Caption = 'Wildcard search'
      TabOrder = 0
    end
    object chbSmartSpaces: TCheckBox
      Left = 12
      Top = 34
      Width = 153
      Height = 17
      Hint = 'Treats multiple spaces, tabs and linefeeds as a single space.'
      Caption = 'Smart spaces'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object chbStartingWith: TCheckBox
      Left = 12
      Top = 53
      Width = 153
      Height = 17
      Hint = 
        'Check to search for strings starting with '#39'Text to find'#39'. Otherw' +
        'ise, any string that contains '#39'Text to find'#39' is returned.'
      Caption = 'Starting with '#39'Text to find'#39
      TabOrder = 2
    end
    object chbStringlistAsOne: TCheckBox
      Left = 12
      Top = 72
      Width = 153
      Height = 17
      Hint = 
        'Check to treat all items in a TStrings as one string. Clear to t' +
        'reat each item as a separate string.'
      Caption = 'Treat stringlist as one string'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
  end
  object mmoHelp: TMemo
    Left = 5
    Top = 215
    Width = 53
    Height = 22
    Lines.Strings = (
      'How to use search:'
      ''
      '=== Wildcard search: ==='
      'Valid wildcards are '
      ' * for any amount of characters'
      ' ? for exactly one character. '
      ''
      'If '#39'Text to find'#39' contains no wildcards, this option is ignored.'
      ''
      
        'A wildcard may be implicitly added to the start of the search st' +
        'ring, depending on the Text to find checkbox. The wildcard searc' +
        'h does NOT add wildcards to the end of the search string.'
      ''
      'To search for a select query using the VAN_SHOP table:'
      'check both Wildcard search and Starting with and search for:'
      ''
      'SELECT*VAN_SHOP*'
      ''
      
        'To search for any query using both VAN_SHOP and VAN_COMPANY tabl' +
        'es (in that order!):'
      'Check Wildcard search and search for:'
      ''
      '*VAN_SHOP *VAN_COMPANY *'
      ''
      '=== Smart spaces ==='
      
        'When Smart spaces is checked, any sequence of white space (space' +
        's, tabs and enters) is treated as if it were one single space ch' +
        'aracter. '
      ''
      '=== Starting with ==='
      
        'If '#39'Starting with text to find'#39' is checked, the dfms will only b' +
        'e searched for strings starting with the specified search string' +
        ', instead of strings containing the string at any position.'
      
        'If this option is not checked during Wildcard search, an asteris' +
        'k (*) is implicitly inserted at the start of the search string. '
      
        'Even if this option is checked, any wildcards at the start of th' +
        'e search string ARE NOT removed.')
    TabOrder = 7
    Visible = False
    WordWrap = False
  end
  object dslSearchDirectory: TJvSelectDirectory
    Left = 112
    Top = 183
  end
end
