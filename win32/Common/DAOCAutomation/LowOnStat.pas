unit LowOnStat;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, DAOCControl;

type
  TfrmLowOnStat = class(TForm)
    chkLowHealth: TCheckBox;
    edtLowHealthPct: TSpinEdit;
    edtLowHealthText: TEdit;
    chkLowEnd: TCheckBox;
    edtLowEndPct: TSpinEdit;
    edtLowEndText: TEdit;
    chkLowMana: TCheckBox;
    edtLowManaPct: TSpinEdit;
    edtLowManaText: TEdit;
    procedure chkLowHealthClick(Sender: TObject);
    procedure chkLowEndClick(Sender: TObject);
    procedure chkLowManaClick(Sender: TObject);
    procedure edtLowHealthPctChange(Sender: TObject);
    procedure edtLowEndPctChange(Sender: TObject);
    procedure edtLowManaPctChange(Sender: TObject);
  private
    FDControl: TDAOCControl;
    FLowHealthTriggered:  boolean;
    FLowEnduranceTriggered:  boolean;
    FLowManaTriggered:    boolean;

    function GetLowEnduranceEnabled: boolean;
    function GetLowEnduranceMessage: string;
    function GetLowEndurancePct: integer;
    function GetLowHealthEnabled: boolean;
    function GetLowHealthPct: integer;
    function GetLowHeathMessage: string;
    function GetLowManaEnabled: boolean;
    function GetLowManaMessage: string;
    function GetLowManaPct: integer;
    procedure SetLowEnduranceEnabled(const Value: boolean);
    procedure SetLowEnduranceMessage(const Value: string);
    procedure SetLowEndurancePct(const Value: integer);
    procedure SetLowHealthEnabled(const Value: boolean);
    procedure SetLowHealthPct(const Value: integer);
    procedure SetLowHeathMessage(const Value: string);
    procedure SetLowManaEnabled(const Value: boolean);
    procedure SetLowManaMessage(const Value: string);
    procedure SetLowManaPct(const Value: integer);
  public
    procedure DAOCLocalHealthUpdate;

    property LowHealthEnabled: boolean read GetLowHealthEnabled write SetLowHealthEnabled;
    property LowHealthPct: integer read GetLowHealthPct write SetLowHealthPct;
    property LowHealthMessage: string read GetLowHeathMessage write SetLowHeathMessage;
    property LowEnduranceEnabled: boolean read GetLowEnduranceEnabled write SetLowEnduranceEnabled;
    property LowEndurancePct: integer read GetLowEndurancePct write SetLowEndurancePct;
    property LowEnduranceMessage: string read GetLowEnduranceMessage write SetLowEnduranceMessage;
    property LowManaEnabled: boolean read GetLowManaEnabled write SetLowManaEnabled;
    property LowManaPct: integer read GetLowManaPct write SetLowManaPct;
    property LowManaMessage: string read GetLowManaMessage write SetLowManaMessage;

    property DAOCControl: TDAOCControl read FDControl write FDControl;
  end;

var
  frmLowOnStat: TfrmLowOnStat;

implementation

{$R *.dfm}

{ TfrmLowOnStat }

function TfrmLowOnStat.GetLowEnduranceEnabled: boolean;
begin
  Result := chkLowEnd.Checked;
end;

function TfrmLowOnStat.GetLowEnduranceMessage: string;
begin
  Result := edtLowEndText.Text;
end;

function TfrmLowOnStat.GetLowEndurancePct: integer;
begin
  Result := edtLowEndPct.Value;
end;

function TfrmLowOnStat.GetLowHealthEnabled: boolean;
begin
  Result := chkLowHealth.Checked;
end;

function TfrmLowOnStat.GetLowHealthPct: integer;
begin
  Result := edtLowHealthPct.Value;
end;

function TfrmLowOnStat.GetLowHeathMessage: string;
begin
  Result := edtLowHealthText.Text;
end;

function TfrmLowOnStat.GetLowManaEnabled: boolean;
begin
  Result := chkLowMana.Checked;
end;

function TfrmLowOnStat.GetLowManaMessage: string;
begin
  Result := edtLowManaText.Text;
end;

function TfrmLowOnStat.GetLowManaPct: integer;
begin
  Result := edtLowManaPct.Value;
end;

procedure TfrmLowOnStat.SetLowEnduranceEnabled(const Value: boolean);
begin
  chkLowEnd.Checked := Value;
  chkLowEndClick(nil);
end;

procedure TfrmLowOnStat.SetLowEnduranceMessage(const Value: string);
begin
  edtLowEndText.Text := Value;
end;

procedure TfrmLowOnStat.SetLowEndurancePct(const Value: integer);
begin
  edtLowEndPct.Value := Value;
end;

procedure TfrmLowOnStat.SetLowHealthEnabled(const Value: boolean);
begin
  chkLowHealth.Checked := Value;
  chkLowHealthClick(nil);
end;

procedure TfrmLowOnStat.SetLowHealthPct(const Value: integer);
begin
  edtLowHealthPct.Value := Value;
end;

procedure TfrmLowOnStat.SetLowHeathMessage(const Value: string);
begin
  edtLowHealthText.Text := Value;
end;

procedure TfrmLowOnStat.SetLowManaEnabled(const Value: boolean);
begin
  chkLowMana.Checked := Value;
  chkLowManaClick(nil);
end;

procedure TfrmLowOnStat.SetLowManaMessage(const Value: string);
begin
  edtLowManaText.Text := Value;
end;

procedure TfrmLowOnStat.SetLowManaPct(const Value: integer);
begin
  edtLowManaPct.Value := Value;
end;

procedure TfrmLowOnStat.chkLowHealthClick(Sender: TObject);
begin
  edtLowHealthPct.Enabled := chkLowHealth.Checked;
  edtLowHealthText.Enabled := chkLowHealth.Checked;
  FLowHealthTriggered := false;
end;

procedure TfrmLowOnStat.chkLowEndClick(Sender: TObject);
begin
  edtLowEndPct.Enabled := chkLowEnd.Checked;
  edtLowEndText.Enabled := chkLowEnd.Checked;
  FLowEnduranceTriggered := false;
end;

procedure TfrmLowOnStat.chkLowManaClick(Sender: TObject);
begin
  edtLowManaPct.Enabled := chkLowMana.Checked;
  edtLowManaText.Enabled := chkLowMana.Checked;
  FLowManaTriggered := false;
end;

procedure TfrmLowOnStat.DAOCLocalHealthUpdate;
begin
  if LowHealthEnabled and
    (FDControl.LocalPlayer.HitPoints <= LowHealthPct) and not FLowHealthTriggered then begin
    FDControl.DoSendKeys(LowHealthMessage + '[cr]');
    FLowHealthTriggered := true;
  end
  else if FDControl.LocalPlayer.HitPoints > LowHealthPct then
    FLowHealthTriggered := false;

  if LowEnduranceEnabled and
    (FDControl.LocalPlayer.HitPoints <= LowEndurancePct) and not FLowEnduranceTriggered then begin
    FDControl.DoSendKeys(LowEnduranceMessage + '[cr]');
    FLowEnduranceTriggered := true;
  end
  else if FDControl.LocalPlayer.EndurancePct > LowEndurancePct then
    FLowEnduranceTriggered := false;

  if LowManaEnabled and
    (FDControl.LocalPlayer.ManaPct <= LowManaPct) and not FLowManaTriggered then begin
    FDControl.DoSendKeys(LowManaMessage + '[cr]');
    FLowManaTriggered := true;
  end
  else if FDControl.LocalPlayer.ManaPct > LowManaPct then
    FLowManaTriggered := false;
end;

procedure TfrmLowOnStat.edtLowHealthPctChange(Sender: TObject);
begin
//  FLowHealthTriggered := false;
end;

procedure TfrmLowOnStat.edtLowEndPctChange(Sender: TObject);
begin
//  FLowEnduranceTriggered := false;
end;

procedure TfrmLowOnStat.edtLowManaPctChange(Sender: TObject);
begin
//  FLowManaTriggered := false;
end;

end.
