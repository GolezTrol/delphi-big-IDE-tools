object FrmSearchProgress: TFrmSearchProgress
  Left = 522
  Top = 355
  BorderStyle = bsDialog
  ClientHeight = 83
  ClientWidth = 400
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    400
    83)
  PixelsPerInch = 96
  TextHeight = 13
  object lblFileName: TLabel
    Left = 8
    Top = 32
    Width = 376
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = '...'
    ExplicitWidth = 185
  end
  object lblSearching: TLabel
    Left = 32
    Top = 8
    Width = 48
    Height = 13
    Caption = 'Searching'
  end
  object lblFound: TLabel
    Left = 8
    Top = 56
    Width = 57
    Height = 13
    Caption = 'Searching...'
  end
  object aniSearch: TAnimate
    Left = 8
    Top = 8
    Width = 16
    Height = 16
    CommonAVI = aviFindFile
    StopFrame = 8
  end
  object tmrShowForm: TTimer
    Enabled = False
    Interval = 1500
    OnTimer = tmrShowFormTimer
    Left = 162
    Top = 8
  end
  object tmrUpdate: TTimer
    Enabled = False
    Interval = 500
    OnTimer = tmrUpdateTimer
    Left = 88
    Top = 8
  end
end
