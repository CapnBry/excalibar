unit MacroTradeSkill;

(****************************************************************************
**
** Copyright (C) 2003 Bryan Mayland.  All rights reserved.
**
** This file may be distributed and/or modified under the terms of the
** GNU General Public License version 2 as published by the Free Software
** Foundation.
**
****************************************************************************)

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DAOCControl, ExtCtrls;

type
  TfrmMacroTradeSkills = class(TForm)
    lblProgression: TLabel;
    edtProgression: TEdit;
    lblTargetQual: TLabel;
    edtTargetQual: TEdit;
    lblTargetSound: TLabel;
    edtTargetSound: TEdit;
    chkStopIfFull: TCheckBox;
    Label1: TLabel;
    edtStackOddsProgression: TEdit;
    lblStackOddsCount: TLabel;
    Bevel1: TBevel;
    edtStackOddsCount: TEdit;
    lblStackOddsProgression: TLabel;
    Label4: TLabel;
    lblStackOddsPct: TLabel;
    edtStackOddsPct: TEdit;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure edtProgressionKeyPress(Sender: TObject; var Key: Char);
    procedure edtTargetQualKeyPress(Sender: TObject; var Key: Char);
    procedure edtTargetSoundKeyPress(Sender: TObject; var Key: Char);
    procedure chkStopIfFullClick(Sender: TObject);
    procedure edtStackOddsProgressionKeyPress(Sender: TObject;
      var Key: Char);
    procedure edtStackOddsCountKeyPress(Sender: TObject; var Key: Char);
    procedure edtStackOddsPctKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
  private
    FDControl: TDAOCControl;
    function GetProgression: string;
    function GetTargetQuality: integer;
    function GetTargetSound: string;
    procedure SetProgression(const Value: string);
    procedure SetTargetQuality(const Value: integer);
    procedure SetTargetSound(const Value: string);
    procedure SetDControl(const Value: TDAOCControl);
    function GetStopIfFull: boolean;
    procedure SetStopIfFull(const Value: boolean);
    function GetOddsLoadCount: integer;
    function GetOddsLoadKey: string;
    procedure SetOddsLoadCount(const Value: integer);
    procedure SetOddsLoadKey(const Value: string);
    function GetOddsLoadPct: double;
    procedure SetOddsLoadPct(const Value: double);
  public
    procedure StartProgression;

    property DAOCControl: TDAOCControl read FDControl write SetDControl;
    property Progression: string read GetProgression write SetProgression;
    property TargetQuality: integer read GetTargetQuality write SetTargetQuality;
    property TargetSound: string read GetTargetSound write SetTargetSound;
    property StopIfFull: boolean read GetStopIfFull write SetStopIfFull;
    property OddsLoadKey: string read GetOddsLoadKey write SetOddsLoadKey;
    property OddsLoadCount: integer read GetOddsLoadCount write SetOddsLoadCount;
    property OddsLoadPct: double read GetOddsLoadPct write SetOddsLoadPct;
  end;

var
  frmMacroTradeSkills: TfrmMacroTradeSkills;

implementation

{$R *.dfm}

{ TfrmMacroTradeSkills }

function TfrmMacroTradeSkills.GetProgression: string;
begin
  Result := edtProgression.Text;
end;

function TfrmMacroTradeSkills.GetTargetQuality: integer;
begin
  Result := StrToIntDef(edtTargetQual.Text, 0);
end;

function TfrmMacroTradeSkills.GetTargetSound: string;
begin
  Result := edtTargetSound.Text;
end;

procedure TfrmMacroTradeSkills.SetDControl(const Value: TDAOCControl);
begin
  if Assigned(FDControl) then
    FDControl.TradeSkillProgression := '';

  FDControl := Value;

  if Assigned(FDControl) then begin
    FDControl.TradeSkillTargetQuality := TargetQuality;
    FDControl.TradeSkillTargetSound := TargetSound;
    FDControl.TradeSkillStopIfFull := StopIfFull;
    FDControl.TradeSkillOddsloadKey := OddsLoadKey;
    FDControl.TradeSkillOddsloadCount := OddsLoadCount;
    FDControl.TradeSkillOddsloadPct := trunc(OddsLoadPct * 10);
  end;

  if Visible and Assigned(FDControl) then
    FDControl.TradeSkillProgression := Progression;
end;

procedure TfrmMacroTradeSkills.SetProgression(const Value: string);
begin
  edtProgression.Text := Value;
  lblProgression.ParentFont := true;

  if Assigned(FDControl) then
    FDControl.TradeSkillProgression := Value;
end;

procedure TfrmMacroTradeSkills.SetTargetQuality(const Value: integer);
begin
  edtTargetQual.Text := IntToStr(Value);
  lblTargetQual.ParentFont := true;

  if Assigned(FDControl) then
    FDControl.TradeSkillTargetQuality := Value;
end;

procedure TfrmMacroTradeSkills.SetTargetSound(const Value: string);
begin
  edtTargetSound.Text := Value;
  lblTargetSound.ParentFont := true;

  if Assigned(FDControl) then
    FDControl.TradeSkillTargetSound := Value;
end;

procedure TfrmMacroTradeSkills.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(FDControl) then
    FDControl.TradeSkillProgression := '';
end;

function TfrmMacroTradeSkills.GetStopIfFull: boolean;
begin
  Result := chkStopIfFull.Checked;
end;

procedure TfrmMacroTradeSkills.SetStopIfFull(const Value: boolean);
begin
  chkStopIfFull.Checked := Value;

  if Assigned(FDControl) then
    FDControl.TradeSkillStopIfFull := Value;
end;

procedure TfrmMacroTradeSkills.edtProgressionKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = #13 then begin
    SetProgression(edtProgression.Text);
    Key := #0;
  end
  else
    lblProgression.Font.Color := clMaroon;
end;

procedure TfrmMacroTradeSkills.edtTargetQualKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = #13 then begin
    SetTargetQuality(StrToInt(edtTargetQual.Text));
    Key := #0;
  end
  else
    lblTargetQual.Font.Color := clMaroon;
end;

procedure TfrmMacroTradeSkills.edtTargetSoundKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = #13 then begin
    SetTargetSound(edtTargetSound.Text);
    Key := #0;
  end
  else
    lblTargetSound.Font.Color := clMaroon;
end;

procedure TfrmMacroTradeSkills.chkStopIfFullClick(Sender: TObject);
begin
  SetStopIfFull(chkStopIfFull.Checked);
end;

function TfrmMacroTradeSkills.GetOddsLoadCount: integer;
begin
  Result := StrToIntDef(edtStackOddsCount.Text, 0);
end;

function TfrmMacroTradeSkills.GetOddsLoadKey: string;
begin
  Result := edtStackOddsProgression.Text;
end;

procedure TfrmMacroTradeSkills.SetOddsLoadCount(const Value: integer);
begin
  edtStackOddsCount.Text := IntToStr(Value);
  lblStackOddsCount.ParentFont := true;

  if Assigned(FDControl) then
    FDControl.TradeSkillOddsloadCount := Value;
end;

procedure TfrmMacroTradeSkills.SetOddsLoadKey(const Value: string);
begin
  edtStackOddsProgression.Text := Value;
  lblStackOddsProgression.ParentFont := true;

  if Assigned(FDControl) then
    FDControl.TradeSkillOddsloadKey := Value;
end;

procedure TfrmMacroTradeSkills.edtStackOddsProgressionKeyPress(
  Sender: TObject; var Key: Char);
begin
  if Key = #13 then begin
    SetOddsLoadKey(edtStackOddsProgression.Text);
    Key := #0;
  end
  else
    lblStackOddsProgression.Font.Color := clMaroon;
end;

procedure TfrmMacroTradeSkills.edtStackOddsCountKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = #13 then begin
    SetOddsLoadCount(StrToInt(edtStackOddsCount.Text));
    Key := #0;
  end
  else
    lblStackOddsCount.Font.Color := clMaroon;
end;

function TfrmMacroTradeSkills.GetOddsLoadPct: double;
begin
  Result := StrToFloatDef(edtStackOddsPct.Text, 0);
end;

procedure TfrmMacroTradeSkills.SetOddsLoadPct(const Value: double);
begin
  edtStackOddsPct.Text := FormatFloat('0.0', Value);
  lblStackOddsPct.ParentFont := true;

  if Assigned(FDControl) then
    FDControl.TradeSkillOddsloadPct := Trunc(Value * 10);
end;

procedure TfrmMacroTradeSkills.edtStackOddsPctKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = #13 then begin
    SetOddsLoadPct(StrToFloat(edtStackOddsPct.Text));
    Key := #0;
  end
  else
    lblStackOddsPct.Font.Color := clMaroon;
end;

procedure TfrmMacroTradeSkills.StartProgression;
begin
  if Assigned(FDControl) then
    FDControl.TradeskillStartProgression;  
end;

procedure TfrmMacroTradeSkills.FormShow(Sender: TObject);
begin
  SetDControl(FDControl);
end;

end.
