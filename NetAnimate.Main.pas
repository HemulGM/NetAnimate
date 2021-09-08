unit NetAnimate.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Generics.Collections, FMX.StdCtrls, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo, System.Math.Vectors, FMX.Types3D, FMX.Controls3D,
  FMX.Objects3D, FMX.Viewport3D, FMX.Effects, FMX.Filter.Effects, FMX.Objects;

type
  TPointItem = record
    Point: TPointF;
    Direct: Single;
    Size: Single;
    Color: TAlphaColor;
  end;

  TPointItems = TList<TPointItem>;

  TFormMain = class(TForm)
    PaintBox1: TPaintBox;
    LabelFPS: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
  private
    FPoint: TPointItems;
    FScreenBounds: TRectF;
    FPS: Integer;
    WorldTicks: Integer;
    procedure Refill;
    function CreateRandomItem: TPointItem;
    function GetPointsCountByFormSize: Int64;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses
  Math;

{$R *.fmx}

function TFormMain.GetPointsCountByFormSize: Int64;
begin
  //������ ���-�� ����� ��� ����� ������� ����
  Result := 2 + (ClientWidth * ClientHeight) div 3500;
end;

procedure TFormMain.Refill;
var
  i: Integer;
begin
  FPoint.Clear;
  //������� ������� ��� ������� � 2 - ������� �����
  for i := 0 to GetPointsCountByFormSize do
    FPoint.Add(CreateRandomItem);
end;

function TFormMain.CreateRandomItem: TPointItem;
begin
  //��������� ��������� ����������
  Result.Point := TPointF.Create(Random(ClientWidth), Random(ClientHeight));
  //��������� ���������� ��������
  Result.Direct := Random(360);
  //������ �����
  Result.Size := RandomRange(2, 5);
  //���� ����� ����������
  TAlphaColorRec(Result.Color).R := RandomRange(40, 250);
  TAlphaColorRec(Result.Color).G := RandomRange(40, 250);
  TAlphaColorRec(Result.Color).B := RandomRange(40, 250);
  TAlphaColorRec(Result.Color).A := 255;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FPoint := TPointItems.Create;
  Randomize;
  //�������� �����, ���������� ������� � ��������
  TThread.CreateAnonymousThread(
    procedure
    var
      i: Integer;
      Item: TPointItem;
    begin
      while not Application.Terminated do
      try
        for i := 1 to FPoint.Count - 1 do
        begin
          //Pull, ��� � ������
          Item := FPoint[i];
          //���� ��������� 10 ��� 20, �� ����� ������������ �� �������
          if Random(50) in [10, 20] then
            Item.Direct := Item.Direct + 0.2;
          //���� ��������� 11 ��� 22, �� ����� ������������ �� ������ �������
          if Random(50) in [11, 22] then
            Item.Direct := Item.Direct - 0.2;
          //��������� ������ �� ���������� ��������
          Item.Point.Offset(Cos(Item.Direct), Sin(Item.Direct));
          //���� ����� �� �������� - ������������� � ��������������� �������
          if Item.Point.X > FScreenBounds.Right then
            Item.Point.X := 0
          else if Item.Point.X < 0 then
            Item.Point.X := FScreenBounds.Right;

          if Item.Point.Y > FScreenBounds.Bottom then
            Item.Point.Y := 0
          else if Item.Point.Y < 0 then
            Item.Point.Y := FScreenBounds.Bottom;
          //Push
          FPoint[i] := Item;
        end;
        //�������
        TThread.Sleep(1);
        Inc(WorldTicks);
      except
        //
      end;
    end).Start;
  //����� ���������
  TThread.CreateAnonymousThread(
    procedure
    begin
      while not Application.Terminated do
      begin
        //������� ����������� ����
        TThread.Synchronize(nil, Invalidate);
        //�������
        TThread.Sleep(1);
        //TThread.Sleep(1000 div 60);
      end;
    end).Start;
  //������ FPS
  TThread.CreateAnonymousThread(
    procedure
    begin
      while not Application.Terminated do
      begin
        //������� ����������� ����
        TThread.Synchronize(nil,
          procedure
          begin
            LabelFPS.Text := FPS.ToString + ' fps ' + WorldTicks.ToString + ' prcs';
            WorldTicks := 0;
            FPS := 0;
          end);
        //�������
        TThread.Sleep(1000);
      end;
    end).Start;
  //{$IFDEF ANDROID}
  FullScreen := True;
  //{$ENDIF}
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FPoint.Free;
end;

procedure TFormMain.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  case Key of
    0:
      Refill;
    vkEscape:
      FullScreen := not FullScreen;
  end;
end;

procedure TFormMain.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
var
  Item: TPointItem;
begin
  //������� ������� ��� �������, �������� ��� ������� ������� �� �����
  Item := FPoint[0];
  Item.Point := TPointF.Create(X, Y);
  FPoint[0] := Item;
  Invalidate;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  //�������� ������� ���� ��� ������������� � ������
  FScreenBounds := ClientRect;
  //������� ��, ��� ��� ���������
  while FPoint.Count > GetPointsCountByFormSize do
    FPoint.Delete(RandomRange(1, FPoint.Count - 1));
  //���������, ���� �� �������
  while FPoint.Count < GetPointsCountByFormSize do
    FPoint.Add(CreateRandomItem);
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  Refill;
end;

procedure TFormMain.PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
const         //��������� ����� �������
  All = 100;  //����� �����
  Me = 150;   //������
var
  i, j: Integer;
  RectPoint: TRectF;
  D: Single;
begin
  with Canvas do
  try
    BeginScene;
    //���
    Fill.Kind := TBrushKind.Solid;
    Fill.Color := $FF101010;
    FillRect(PaintBox1.BoundsRect, 0, 0, [], 1.0, TCornerType.Round);
    //����� ���������
    Fill.Kind := TBrushKind.Solid;
    Fill.Color := TAlphaColorRec.White;
    Stroke.Kind := TBrushKind.Solid;
    //�� ���� ��������
    for i := 0 to FPoint.Count - 1 do
    begin
      //������� - ������ � ���� ������ ��� ���� - �������
      if (i = 0) and (not ClientRect.Contains(ScreenToClient(Screen.MousePos))) then
        Continue;
      //���� ������ ������� ������� ������
      for j := 0 to FPoint.Count - 1 do
      begin
        //���� ������ ���� �� ������. �� ��� ���� ��� ��� �� ��������� �� ���.
        if i < j then
        begin
          //��������� ����� �������
          D := FPoint[i].Point.Distance(FPoint[j].Point);
          //���� �� ������
          if i <> 0 then
          begin
            //���� ��������� �� ������� �������
            if D < All then
            begin
              //����� ����
              Stroke.Color := FPoint[j].Color;
              Stroke.Thickness := FPoint[j].Size;
              //������ � ������ ���������� ����� �������, "((100 / All) * Abs(All - D)) / 100" - ��� ������������ �����
              DrawLine(FPoint[i].Point, FPoint[j].Point, ((100 / All) * Abs(All - D)) / 100);
            end;
          end
          else
          begin
            if D < Me then
            begin
              //����� ����
              Stroke.Color := FPoint[j].Color;
              Stroke.Thickness := FPoint[j].Size;
              //������ � ������ ���������� ����� �������
              DrawLine(FPoint[i].Point, FPoint[j].Point, ((100 / Me) * Abs(Me - D)) / 100);
            end;
          end;
        end;
      end;
      //� ��� � ��� ���� ����� - �������
      //���������� �������
      RectPoint := TRectF.Create(FPoint[i].Point, FPoint[i].Size, FPoint[i].Size);
      //������� � ������ ������
      RectPoint.Offset(-FPoint[i].Size / 2, -FPoint[i].Size / 2);
      //������
      FillEllipse(RectPoint, 0.6);
    end;
  finally
    EndScene;
  end;
  Inc(FPS);
end;

end.

