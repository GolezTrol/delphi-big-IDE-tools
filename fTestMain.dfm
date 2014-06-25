object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 394
  ClientWidth = 358
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    358
    394)
  PixelsPerInch = 96
  TextHeight = 13
  object Edit1: TEdit
    Left = 8
    Top = 8
    Width = 342
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'C:\Projects\Vanessa2\Source\dProductAssortment.dfm'
  end
  object Button1: TButton
    Left = 8
    Top = 65
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Edit2: TEdit
    Left = 8
    Top = 35
    Width = 233
    Height = 21
    TabOrder = 2
    Text = 'EXPECTEDRELEASEDATE'
  end
  object Memo1: TMemo
    Left = 8
    Top = 96
    Width = 342
    Height = 290
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    TabOrder = 3
  end
end
