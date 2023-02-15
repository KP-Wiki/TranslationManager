object Form1: TForm1
  Left = 230
  Top = 140
  Caption = 'Translation Manager'
  ClientHeight = 600
  ClientWidth = 993
  Color = clBtnFace
  Constraints.MinHeight = 600
  Constraints.MinWidth = 800
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Calibri'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 19
  object pnBody: TPanel
    Left = 0
    Top = 65
    Width = 993
    Height = 516
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      993
      516)
    object Label1: TLabel
      Left = 8
      Top = 0
      Width = 25
      Height = 18
      Caption = 'Libx'
    end
    object Label2: TLabel
      Left = 336
      Top = 0
      Width = 64
      Height = 18
      Caption = 'Languages'
      Transparent = True
    end
    object lbTagName: TLabel
      Left = 488
      Top = 0
      Width = 91
      Height = 18
      Caption = 'TX_TAG_NAME'
    end
    object Label4: TLabel
      Left = 336
      Top = 176
      Width = 95
      Height = 18
      Caption = 'Tag name filter:'
    end
    object Label5: TLabel
      Left = 8
      Top = 248
      Width = 26
      Height = 18
      Caption = 'Tags'
    end
    object Label3: TLabel
      Left = 336
      Top = 224
      Width = 109
      Height = 18
      Caption = 'English text filter:'
    end
    object ScrollBox1: TScrollBox
      Left = 488
      Top = 16
      Width = 497
      Height = 460
      HorzScrollBar.Visible = False
      VertScrollBar.Smooth = True
      VertScrollBar.Tracking = True
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelEdges = []
      BevelInner = bvNone
      BevelOuter = bvNone
      TabOrder = 0
    end
    object lbLibs: TListBox
      Left = 8
      Top = 16
      Width = 321
      Height = 225
      ItemHeight = 19
      TabOrder = 1
      OnClick = lbLibsClick
    end
    object clbShowLang: TCheckListBox
      Left = 336
      Top = 16
      Width = 145
      Height = 153
      OnClickCheck = clbShowLangClickCheck
      AutoComplete = False
      Columns = 3
      ItemHeight = 19
      TabOrder = 2
    end
    object btnCopy: TButton
      Left = 488
      Top = 483
      Width = 121
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Copy all strings'
      TabOrder = 3
      OnClick = btnCopyClick
    end
    object btnEraseAllButEng: TButton
      Left = 744
      Top = 483
      Width = 161
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Erase all but English'
      Default = True
      TabOrder = 4
      OnClick = btnEraseAllButEngClick
    end
    object btnPaste: TButton
      Left = 616
      Top = 483
      Width = 121
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Paste all strings'
      Enabled = False
      TabOrder = 5
      OnClick = btnPasteClick
    end
    object btnInsert: TButton
      Left = 8
      Top = 456
      Width = 121
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Insert New'
      TabOrder = 6
      OnClick = btnInsertClick
    end
    object btnInsertSeparator: TButton
      Left = 8
      Top = 480
      Width = 121
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Insert Separator'
      TabOrder = 7
      OnClick = btnInsertSeparatorClick
    end
    object btnRename: TButton
      Left = 136
      Top = 456
      Width = 81
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Rename'
      TabOrder = 9
      OnClick = btnRenameClick
    end
    object btnMoveUp: TButton
      Left = 224
      Top = 456
      Width = 105
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Move Up'
      TabOrder = 10
      OnClick = btnMoveUpClick
    end
    object btnMoveDown: TButton
      Left = 224
      Top = 480
      Width = 105
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Move Down'
      TabOrder = 11
      OnClick = btnMoveDownClick
    end
    object lbTags: TListBox
      Left = 8
      Top = 264
      Width = 321
      Height = 185
      Anchors = [akLeft, akTop, akBottom]
      ItemHeight = 19
      TabOrder = 12
      OnClick = lbTagsClick
      OnKeyPress = lbTagsKeyPress
    end
    object cbFilterDuplicateTexts: TCheckBox
      Left = 336
      Top = 304
      Width = 145
      Height = 17
      Caption = 'Filter repeating texts'
      TabOrder = 13
      OnClick = cbFilterEmptyTextsClick
    end
    object cbFilterEmptyTexts: TCheckBox
      Left = 336
      Top = 288
      Width = 137
      Height = 17
      Caption = 'Filter empty texts'
      TabOrder = 14
      OnClick = cbFilterEmptyTextsClick
    end
    object edFilterEngText: TEdit
      Left = 336
      Top = 240
      Width = 145
      Height = 26
      TabOrder = 15
      OnChange = edFilterEngTextChange
    end
    object btnDelete: TButton
      Left = 136
      Top = 480
      Width = 81
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Delete'
      TabOrder = 8
      OnClick = btnDeleteClick
    end
    object edFilterTagName: TEdit
      Left = 336
      Top = 192
      Width = 145
      Height = 26
      TabOrder = 16
      OnChange = edFilterEngTextChange
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 581
    Width = 993
    Height = 19
    Panels = <
      item
        Width = 100
      end
      item
        Width = 50
      end>
  end
  object pnTop: TPanel
    Left = 0
    Top = 0
    Width = 993
    Height = 65
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object btnCompactIndexes: TButton
      Left = 816
      Top = 8
      Width = 129
      Height = 25
      Caption = 'Compact indexes'
      TabOrder = 0
      OnClick = btnCompactIndexesClick
    end
    object btnCopyToClipboard: TButton
      Left = 368
      Top = 8
      Width = 169
      Height = 25
      Caption = 'Copy to clipboard'
      TabOrder = 1
      OnClick = btnCopyToCBClick
    end
    object btnListUnusedTags: TButton
      Left = 680
      Top = 8
      Width = 129
      Height = 25
      Caption = 'List unused tags'
      TabOrder = 2
      OnClick = btnListUnusedTagsClick
    end
    object btnPasteFromClipboard: TButton
      Left = 368
      Top = 32
      Width = 169
      Height = 25
      Caption = 'Paste from clipboard'
      TabOrder = 3
      OnClick = btnPasteFromCBClick
    end
    object btnSortByIndex: TButton
      Left = 544
      Top = 8
      Width = 129
      Height = 25
      Caption = 'Sort by index'
      TabOrder = 4
      OnClick = btnSortByIndexClick
    end
    object btnSortByTag: TButton
      Left = 544
      Top = 32
      Width = 129
      Height = 25
      Caption = 'Sort by tag'
      TabOrder = 5
      OnClick = btnSortByTagClick
    end
    object btnCopyToClipboardAll: TButton
      Left = 192
      Top = 8
      Width = 169
      Height = 25
      Caption = 'Copy ALL to clipboard'
      TabOrder = 7
      OnClick = btnCopyToClipboardAllClick
    end
    object btnListMismatching: TButton
      Left = 680
      Top = 32
      Width = 129
      Height = 25
      Caption = 'List mismatching'
      TabOrder = 8
      OnClick = btnListMismatchingClick
    end
    object btnPasteFromClipboardAll: TButton
      Left = 192
      Top = 32
      Width = 169
      Height = 25
      Caption = 'Paste ALL from clipboard'
      TabOrder = 6
      OnClick = btnPasteFromClipboardAllClick
    end
    object btnSave: TButton
      Left = 8
      Top = 8
      Width = 89
      Height = 25
      Caption = 'Save'
      TabOrder = 10
      OnClick = btnSaveClick
    end
    object btnExit: TButton
      Left = 8
      Top = 32
      Width = 89
      Height = 25
      Caption = 'Exit'
      TabOrder = 9
      OnClick = btnExitClick
    end
  end
end
