object Form1: TForm1
  Left = 230
  Top = 140
  Caption = 'Translation Manager'
  ClientHeight = 753
  ClientWidth = 993
  Color = clBtnFace
  Constraints.MinHeight = 600
  Constraints.MinWidth = 800
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnCloseQuery = FormCloseQuery
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  DesignSize = (
    993
    753)
  PixelsPerInch = 96
  TextHeight = 16
  object Label1: TLabel
    Left = 8
    Top = 0
    Width = 22
    Height = 16
    Caption = 'Libx'
  end
  object Label2: TLabel
    Left = 336
    Top = 0
    Width = 61
    Height = 16
    Caption = 'Languages'
    Transparent = True
  end
  object Label3: TLabel
    Left = 336
    Top = 304
    Width = 60
    Height = 16
    Caption = 'Text filter:'
  end
  object Label4: TLabel
    Left = 336
    Top = 224
    Width = 211
    Height = 16
    Caption = 'Tag name filter (*TAG, TAG, TAG*):'
  end
  object Label5: TLabel
    Left = 8
    Top = 248
    Width = 28
    Height = 16
    Caption = 'Tags'
  end
  object lbTagName: TLabel
    Left = 568
    Top = 0
    Width = 87
    Height = 16
    Caption = 'TX_TAG_NAME'
  end
  object Label6: TLabel
    Left = 336
    Top = 360
    Width = 28
    Height = 16
    Caption = 'Tags'
  end
  object Label7: TLabel
    Left = 336
    Top = 512
    Width = 66
    Height = 16
    Caption = 'Current libx'
  end
  object Label8: TLabel
    Left = 336
    Top = 608
    Width = 37
    Height = 16
    Caption = 'All libx'
  end
  object Label9: TLabel
    Left = 336
    Top = 264
    Width = 150
    Height = 16
    Caption = 'Tag ID filter (1,2,3, 8-15):'
  end
  object btnCompactIndexes: TButton
    Left = 336
    Top = 456
    Width = 225
    Height = 25
    Caption = 'Compact indexes'
    TabOrder = 0
    OnClick = btnCompactIndexesClick
  end
  object btnCopy: TButton
    Left = 568
    Top = 720
    Width = 121
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Copy all strings'
    TabOrder = 1
    OnClick = btnCopyClick
  end
  object btnCopyToClipboard: TButton
    Left = 336
    Top = 528
    Width = 225
    Height = 25
    Caption = 'Copy to clipboard'
    TabOrder = 2
    OnClick = btnCopyToCBClick
  end
  object btnCopyToClipboardAll: TButton
    Left = 336
    Top = 624
    Width = 225
    Height = 25
    Caption = 'Copy ALL to clipboard'
    TabOrder = 3
    OnClick = btnCopyToClipboardAllClick
  end
  object btnEraseAllButEng: TButton
    Left = 824
    Top = 720
    Width = 161
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Erase all but English'
    Default = True
    TabOrder = 5
    OnClick = btnEraseAllButEngClick
  end
  object btnPaste: TButton
    Left = 696
    Top = 720
    Width = 121
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Paste all strings'
    Enabled = False
    TabOrder = 12
    OnClick = btnPasteClick
  end
  object btnPasteFromClipboard: TButton
    Left = 336
    Top = 552
    Width = 225
    Height = 25
    Caption = 'Paste from clipboard'
    TabOrder = 13
    OnClick = btnPasteFromCBClick
  end
  object btnPasteFromClipboardAll: TButton
    Left = 336
    Top = 648
    Width = 225
    Height = 25
    Caption = 'Paste ALL from clipboard'
    TabOrder = 14
    OnClick = btnPasteFromClipboardAllClick
  end
  object btnSave: TButton
    Left = 336
    Top = 712
    Width = 225
    Height = 33
    Caption = 'Save'
    TabOrder = 16
    OnClick = btnSaveClick
  end
  object cbFilterDuplicateTexts: TCheckBox
    Left = 336
    Top = 184
    Width = 145
    Height = 17
    Caption = 'Filter repeating texts'
    TabOrder = 19
    OnClick = cbFilterEmptyTextsClick
  end
  object cbFilterEmptyTexts: TCheckBox
    Left = 336
    Top = 168
    Width = 137
    Height = 17
    Caption = 'Filter empty texts'
    TabOrder = 20
    OnClick = cbFilterEmptyTextsClick
  end
  object cbFilterMismatching: TCheckBox
    Left = 336
    Top = 200
    Width = 153
    Height = 17
    Caption = 'Filter mismatching texts'
    TabOrder = 21
    OnClick = cbFilterEmptyTextsClick
  end
  object clbShowLang: TCheckListBox
    Left = 336
    Top = 16
    Width = 225
    Height = 145
    OnClickCheck = clbShowLangClickCheck
    AutoComplete = False
    Columns = 4
    TabOrder = 22
  end
  object edFilterText: TEdit
    Left = 336
    Top = 320
    Width = 225
    Height = 24
    TabOrder = 23
    OnChange = edFilterTextChange
  end
  object edFilterTagName: TEdit
    Left = 336
    Top = 240
    Width = 225
    Height = 24
    TabOrder = 24
    OnChange = edFilterTextChange
  end
  object lbLibs: TListBox
    Left = 8
    Top = 80
    Width = 321
    Height = 161
    TabOrder = 25
    OnClick = lbLibsClick
  end
  object lbTags: TListBox
    Left = 8
    Top = 264
    Width = 321
    Height = 483
    Anchors = [akLeft, akTop, akBottom]
    TabOrder = 26
    OnClick = lbTagsClick
    OnKeyPress = lbTagsKeyPress
  end
  object ScrollBox1: TScrollBox
    Left = 568
    Top = 16
    Width = 417
    Height = 697
    HorzScrollBar.Visible = False
    VertScrollBar.Smooth = True
    VertScrollBar.Tracking = True
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelEdges = []
    BevelInner = bvNone
    BevelOuter = bvNone
    TabOrder = 27
  end
  object btnInsert: TButton
    Left = 336
    Top = 376
    Width = 113
    Height = 25
    Caption = 'Insert New'
    TabOrder = 6
    OnClick = btnInsertClick
  end
  object btnInsertSeparator: TButton
    Left = 448
    Top = 376
    Width = 113
    Height = 25
    Caption = 'Insert Separator'
    TabOrder = 7
    OnClick = btnInsertSeparatorClick
  end
  object btnRename: TButton
    Left = 336
    Top = 400
    Width = 113
    Height = 25
    Caption = 'Rename'
    TabOrder = 15
    OnClick = btnRenameClick
  end
  object btnDelete: TButton
    Left = 448
    Top = 400
    Width = 113
    Height = 25
    Caption = 'Delete'
    TabOrder = 4
    OnClick = btnDeleteClick
  end
  object btnMoveUp: TButton
    Left = 336
    Top = 424
    Width = 113
    Height = 25
    Caption = 'Move Up'
    TabOrder = 11
    OnClick = btnMoveUpClick
  end
  object btnMoveDown: TButton
    Left = 448
    Top = 424
    Width = 113
    Height = 25
    Caption = 'Move Down'
    TabOrder = 10
    OnClick = btnMoveDownClick
  end
  object btnSortByIndex: TButton
    Left = 336
    Top = 480
    Width = 113
    Height = 25
    Caption = 'Sort by index'
    TabOrder = 17
    OnClick = btnSortByIndexClick
  end
  object btnSortByTag: TButton
    Left = 448
    Top = 480
    Width = 113
    Height = 25
    Caption = 'Sort by tag'
    TabOrder = 18
    OnClick = btnSortByTagClick
  end
  object btnListUnusedTags: TButton
    Left = 336
    Top = 576
    Width = 225
    Height = 25
    Caption = 'List unused tags'
    TabOrder = 9
    OnClick = btnListUnusedTagsClick
  end
  object btnListMismatching: TButton
    Left = 336
    Top = 672
    Width = 225
    Height = 25
    Caption = 'List ALL mismatching'
    TabOrder = 8
    OnClick = btnListMismatchingClick
  end
  object cbLibxDomains: TCheckListBox
    Left = 8
    Top = 16
    Width = 321
    Height = 57
    OnClickCheck = clbShowLangClickCheck
    AutoComplete = False
    Columns = 3
    TabOrder = 28
    OnClick = cbLibxDomainsClick
  end
  object edFilterTagId: TEdit
    Left = 336
    Top = 280
    Width = 225
    Height = 24
    TabOrder = 29
    OnChange = edFilterTextChange
  end
  object btnSaveToZip: TButton
    Left = 384
    Top = 584
    Width = 225
    Height = 25
    Caption = 'Save to ZIP'
    TabOrder = 30
    OnClick = btnSaveToZipClick
  end
  object btnSaveAllToZip: TButton
    Left = 368
    Top = 688
    Width = 225
    Height = 25
    Caption = 'Save ALL to ZIP'
    TabOrder = 31
    OnClick = btnSaveAllToZipClick
  end
  object sdExportZIP: TSaveDialog
    DefaultExt = 'zip'
    Filter = 'ZIP file|*.zip'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 344
    Top = 24
  end
end
