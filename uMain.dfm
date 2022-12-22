object Form2: TForm2
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Sierpinski triangle generator'
  ClientHeight = 435
  ClientWidth = 548
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object imSierpinski: TImage
    Left = 0
    Top = 0
    Width = 409
    Height = 435
    Align = alLeft
    ExplicitLeft = -6
    ExplicitHeight = 425
  end
  object pnControls: TPanel
    Left = 409
    Top = 0
    Width = 139
    Height = 435
    Align = alClient
    Caption = 'pnControls'
    ShowCaption = False
    TabOrder = 0
    object btnInitialPicker: TButton
      Left = 23
      Top = 168
      Width = 89
      Height = 25
      Caption = 'Initialize'
      TabOrder = 0
      OnClick = btnInitialPickerClick
    end
    object btnPointPicker: TButton
      Left = 23
      Top = 251
      Width = 89
      Height = 25
      Caption = 'Pick point(s)'
      TabOrder = 1
      OnClick = btnPointPickerClick
    end
    object btnReset: TButton
      Left = 23
      Top = 112
      Width = 89
      Height = 25
      Caption = 'Reset'
      TabOrder = 2
      OnClick = btnResetClick
    end
    object edtPointCount: TEdit
      Left = 23
      Top = 216
      Width = 57
      Height = 21
      NumbersOnly = True
      TabOrder = 3
      Text = '1'
    end
  end
end
