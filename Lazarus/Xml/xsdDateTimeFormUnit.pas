unit xsdDateTimeFormUnit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, SysUtils, Grids, Calendar, Spin, FormIniFilez;

type TdtFormat = (dtfDateTime, dtfDate);
type

  { TxsdDateTimeForm }

  TxsdDateTimeForm = class(TForm)
    OKButton: TButton;
    TimeZoneEdir: TBevel;
    YearEdit: TLabeledEdit;
    MonthEdit: TLabel;
    MonthComboBox: TComboBox;
    Calendar: TCalendar;
    NowButton: TButton;
    Label1: TLabel;
    TimeEdit: TLabeledEdit;
    TimeZoneEdit: TLabeledEdit;
    Button2: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure NowButtonClick(Sender: TObject);
    procedure YearEditChange(Sender: TObject);
    procedure MonthComboBoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    IniFile: TFormIniFile;
    fdtFormat: TdtFormat;
    fXsdDateTime: String;
    procedure setDtFormat(const Value: TdtFormat);
    procedure setXsdDateTime(Value: String);
  public
    property dtFormat: TdtFormat read fdtFormat write setDtFormat;
    property xsdDateTime: String read fXsdDateTime write setXsdDateTime;
  end;

var
  xsdDateTimeForm: TxsdDateTimeForm;

implementation

uses RegExpr
   , xmlxsdparser
   ;


{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}
const
  dtregexp = '^[0-9]{4}\-[0-9]{2}\-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(\.([0-9]{1,9}))?(Z|([\+\-][0-9]{2}:[0-9]{2}))?$';
  dregexp = '^[0-9]{4}\-[0-9]{2}\-[0-9]{2}$';
  tregexp = '^[0-9]{2}:[0-9]{2}:[0-9]{2}(\.([0-9]{1,9}))?$';
  zregexp = '^(Z|([\+\-][0-9]{2}:[0-9]{2}))?$';

procedure TxsdDateTimeForm.FormCreate(Sender: TObject);
var
  x: Integer;
begin
  IniFile := TFormIniFile.Create (Self);
  dtFormat := dtfDateTime;
  MonthComboBox.Clear;
  for x := 1 to 12 do
    MonthComboBox.Items.Add(LongMonthNames [x]);
end;

procedure TxsdDateTimeForm.setXsdDateTime(Value: String);
var
  x: Integer;
  rx: TRegExpr;
  dt: TDateTime;
  eeyy, mm, dd: Word;
  part: Integer;
begin
  if Value = '' then
  begin
    if dtFormat = dtfDateTime then
      Value := xsdFormatDateTime(Now, nil)
    else
      Value := xsdFormatDate(Now, nil);
  end;
  DecodeDate(now, eeyy, mm, dd);
  TimeEdit.Text := '00:00:00.000';
  TimeZoneEdit.Text := '';
  rx := TRegExpr.Create;
  try
    try
      if dtFormat = dtfDateTime then
      begin
        rx.Expression := dtregexp;
        if rx.Exec(Value) then
        begin
    //    2010-09-08T07:06:05.43210+01:00
    //    1234567890123456789012345678901
          eeyy := StrToInt (Copy (Value,  1, 4));
          mm := StrToInt (Copy (Value,  6, 2));
          dd := StrToInt (Copy (Value,  9, 2));
          x := 20;
          if (Length (Value) > 10) and (Value [11] = 'T') then
          begin
            if Length (Value) > 19 then
            begin
              if Value [20] = '.' then
              begin
                x := 21;
                while (x <= Length (Value))
                and (Value [x] >= '0')
                and (Value [x] <= '9')
                do
                  Inc (x);
              end;
              TimeEdit.Text := Copy (Value , 12, x - 12);
              TimeZoneEdit.Text := Copy (Value , x, 6);
            end;
          end;
        end;
      end;
      if dtFormat = dtfDate then
      begin
        rx.Expression := dregexp;
        if rx.Exec(Value) then
        begin
    //    2010-09-08
    //    1234567890
          eeyy := StrToInt (Copy (Value,  1, 4));
          mm := StrToInt (Copy (Value,  6, 2));
          dd := StrToInt (Copy (Value,  9, 2));
        end;
      end;
      if dtFormat = dtfDateTime then
      begin
        rx.Expression := dregexp;
        if rx.Exec(Value) then
        begin
    //    2010-09-08
    //    1234567890
          eeyy := StrToInt (Copy (Value,  1, 4));
          mm := StrToInt (Copy (Value,  6, 2));
          dd := StrToInt (Copy (Value,  9, 2));
        end;
      end;
    //    2010-09-08T07:06:05.43210+01:00
    //    1234567890123456789012345678901
    except
    end;
  finally
    rx.Free;
    yearedit.Text := IntToStr (eeyy);
    MonthComboBox.ItemIndex := mm - 1;
    Calendar.Date := Copy (Value, 1, 10);
  end;
end;

procedure TxsdDateTimeForm.MonthComboBoxChange(Sender: TObject);
var
  eeyy, mm, dd: Word;
begin
  try
    DecodeDate(Calendar.DateTime, eeyy, mm,dd);
    mm := MonthComboBox.ItemIndex + 1;
    Calendar.DateTime := EncodeDate(eeyy, mm, dd);
  except
  end;
end;

procedure TxsdDateTimeForm.YearEditChange(Sender: TObject);
var
  eeyy, mm, dd: Word;
begin
  if Length (YearEdit.Text) = 4 then
    try
      DecodeDate(Calendar.DateTime, eeyy, mm,dd);
      eeyy := StrToIntDef (YearEdit.Text, 0);
      Calendar.DateTime := EncodeDate(eeyy, mm, dd);
    except
    end;
end;

procedure TxsdDateTimeForm.NowButtonClick(Sender: TObject);
var
  eeyy, mm, dd, hh, mn, ss, ms: Word;
  t: String;
begin
  DecodeDate(now, eeyy, mm, dd);
  DecodeTime(now, hh, mn, ss, ms);
  yearedit.Text := IntToStr (eeyy);
  MonthComboBox.ItemIndex := mm - 1;
  Calendar.DateTime := Now;
  t := '';
  if hh < 10 then t := t + '0';
  t := t + IntToStr (hh) + ':';
  if mn < 10 then t := t + '0';
  t := t + IntToStr (mn) + ':';
  if ss < 10 then t := t + '0';
  t := t + IntToStr (ss) + '.';
  t := t + IntToStr (ms);
  TimeEdit.Text := t;
  OkButton.OnClick (nil);
end;

procedure TxsdDateTimeForm.setDtFormat(const Value: TdtFormat);
begin
  fdtFormat := Value;
  TimeEdit.Visible := (Value = dtfDateTime);
  TimeZoneEdit.Visible := (Value = dtfDateTime);
end;

procedure TxsdDateTimeForm.OKButtonClick(Sender: TObject);
var
  dt: TDateTime;
  rx: TRegExpr;
  rValue: String;
  iValue: Integer;
begin
  ModalResult := mrNone;
{$ifdef NOTDEF}
  rx := TRegExpr.Create;
  try
    rx.Expression := '[0-9]{4}';
    if not rx.Exec(YearEdit.Text) then
    begin
      YearEdit.SetFocus;
      raise Exception.Create('Illegal format');
    end;
    if (MonthComboBox.ItemIndex < 0 ) then
    begin
      MonthComboBox.SetFocus;
      raise Exception.Create('Illegal value');
    end;
    rValue := YearEdit.Text + '-';
    if Calendar.Month < 10 then
      rValue := rValue + '0';
    rValue := rValue + IntToStr (Calendar.Month) + '-';
    if Calendar.Day < 10 then
      rValue := rValue + '0';
    rValue := rValue + IntToStr (Calendar.Day);
    rx.Expression := dregexp;
    if not rx.Exec(rValue) then
      raise Exception.Create('Illegal date: ' + rValue);
    if dtFormat = dtfDateTime then
    begin
      rx.Expression := tregexp;
      if not rx.Exec(TimeEdit.Text) then
      begin
        TimeEdit.SetFocus;
        raise Exception.Create('Illegal value');
      end;
//      00:00:00
//      12345678
      if StrToInt (Copy (TimeEdit.Text, 1, 2)) > 23 then
      begin
        TimeEdit.SetFocus;
        raise Exception.Create('Illegal value');
      end;
      if StrToInt (Copy (TimeEdit.Text, 4, 2)) > 59 then
      begin
        TimeEdit.SetFocus;
        raise Exception.Create('Illegal value');
      end;
      if StrToInt (Copy (TimeEdit.Text, 7, 2)) > 59 then
      begin
        TimeEdit.SetFocus;
        raise Exception.Create('Illegal value');
      end;
      rValue := rValue + 'T' + TimeEdit.Text;
      rx.Expression := zregexp;
      if not rx.Exec(TimeZoneEdit.Text) then
      begin
        TimeZoneEdit.SetFocus;
        raise Exception.Create('Illegal value');
      end;
      rValue := rValue + TimeZoneEdit.Text;
    end;
    fXsdDateTime := rValue;
    ModalResult := mrOK;
  finally
    rx.Free;
  end;
  {$else}
  if dtFormat = dtfDate then
    fXsdDateTime := xsdFormatDate(Calendar.DateTime, nil)
  else
    fXsdDateTime := xsdFormatDate(Calendar.DateTime) + 'T' + TimeEdit.Text;
  ModalResult := mrOK;
  {$endif}
end;

procedure TxsdDateTimeForm.FormShow(Sender: TObject);
begin
  if dtFormat = dtfDateTime then
    NowButton.Caption := 'No&w'
  else
    NowButton.Caption := 'Tod&ay';
end;

procedure TxsdDateTimeForm.FormDestroy(Sender: TObject);
begin
  IniFile.Free;
end;


end.

