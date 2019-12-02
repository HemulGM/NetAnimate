unit NetAnimate.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Generics.Collections, FMX.StdCtrls, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo;

type
  TPointItem = record
    Point: TPointF;
    Direct: Single;
    Size: Single;
    Color: TAlphaColor;
  end;

  TPointItems = TList<TPointItem>;

  TFormMain = class(TForm)
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FPoint: TPointItems;
    FScreenBounds: TRectF;
    procedure Refill;
    function CreateRandomItem: TPointItem;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses
  Math;

{$R *.fmx}

procedure TFormMain.Refill;
var
  i: Integer;
begin
  FPoint.Clear;
  //(ClientWidth * ClientHeight) div 3500 - это расчет кол-ва точек при нашем разрешении экрана
  //Нолевой элемент для курсора
  for i := 0 to (ClientWidth * ClientHeight) div 3500 do
    FPoint.Add(CreateRandomItem);
end;

function TFormMain.CreateRandomItem: TPointItem;
begin
  //Начальные случайные координаты
  Result.Point := TPointF.Create(Random(ClientWidth), Random(ClientHeight));
  //Начальное напрвление движения
  Result.Direct := Random(360);
  //Размер точки
  Result.Size := RandomRange(2, 5);
  //Цвет линии соединения
  TAlphaColorRec(Result.Color).R := RandomRange(40, 250);
  TAlphaColorRec(Result.Color).G := RandomRange(40, 250);
  TAlphaColorRec(Result.Color).B := RandomRange(40, 250);
  TAlphaColorRec(Result.Color).A := 255;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  {$IFDEF ANDROID}
  FullScreen := True;
  {$ENDIF}
  FPoint := TPointItems.Create;
  Randomize;
  //Основной поток, приводящий объекты в движение
  TThread.CreateAnonymousThread(
    procedure
    var
      i: Integer;
      Item: TPointItem;
    begin
      while not Application.Terminated do
      begin
        for i := 1 to FPoint.Count - 1 do
        begin
          //Pull, это ж запись
          Item := FPoint[i];
          //Если случайное 10 или 20, то чуток поворачиваем по часовой
          if Random(50) in [10, 20] then
            Item.Direct := Item.Direct + 0.2;
          //Если случайное 11 или 22, то чуток поворачиваем по против часовой
          if Random(50) in [11, 22] then
            Item.Direct := Item.Direct - 0.2;
          //Смещаемся исходя из троектории движения
          Item.Point.Offset(Cos(Item.Direct), Sin(Item.Direct));
          //Если точка за границей - телепортируем к противололожной границе
          if Item.Point.X > FScreenBounds.Right then
            Item.Point.X := 0;
          if Item.Point.X < 0 then
            Item.Point.X := FScreenBounds.Right;

          if Item.Point.Y > FScreenBounds.Bottom then
            Item.Point.Y := 0;
          if Item.Point.Y < 0 then
            Item.Point.Y := FScreenBounds.Bottom;
          //Push
          FPoint[i] := Item;
        end;
        //Требуем перерисовки окна
        TThread.Synchronize(TThread.CurrentThread,
          procedure
          begin
            Invalidate;
          end);
        //Отдохнём 30 мсек.
        Sleep(30);
      end;
    end).Start;
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
    13:
      FullScreen := True;
    27:
      FullScreen := False;
  end;
end;

procedure TFormMain.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
var
  Item: TPointItem;
begin
  //Нолувой жлемент для курсора, присвоим ему зпозицию курсора на форме
  Item := FPoint[0];
  Item.Point := TPointF.Create(X, Y);
  FPoint[0] := Item;
  //Нужна перерисовка
  Invalidate;
end;

procedure TFormMain.FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
const         //Дистанция между точками
  All = 100;  //Любые точки
  Me = 150;   //Курсор
var
  i, j: Integer;
  RectPoint: TRectF;
  D: Single;
begin
  with Canvas do
  begin
    BeginScene;
    //Фон
    Fill.Color := $FF101010;
    FillRect(ARect, 0, 0, [], 1.0, TCornerType.Round);
    //Общие настройки
    Fill.Kind := TBrushKind.Solid;
    Fill.Color := TAlphaColorRec.White;
    Stroke.Kind := TBrushKind.Solid;
    //По всем объектам
    for i := 0 to FPoint.Count - 1 do
    begin
      //Нулевой - крусор
      if i = 0 then
      begin
        //Если курсор вне окна - игонрим
        if not ClientRect.Contains(ScreenToClient(Screen.MousePos)) then
          Continue;
      end;
      //Ищем нашему объекту близкий объект
      for j := 0 to FPoint.Count - 1 do
      begin
        //Если объект выше по списку. Те что выше нас уже всё проверили до нас.
        if i < j then
        begin
          //Дистанция между точками
          D := FPoint[i].Point.Distance(FPoint[j].Point);
          //Если не курсор
          if i <> 0 then
          begin
            //Если дистанция не слишком большая
            if D < All then
            begin
              //Задаём цвет
              Stroke.Color := FPoint[j].Color;
              //Рисуем с учетом расстояния между точками, "((100 / All) * Abs(All - D)) / 100" - это прозрачность линии
              DrawLine(FPoint[i].Point, FPoint[j].Point, ((100 / All) * Abs(All - D)) / 100);
            end;
          end
          else
          begin
            if D < Me then
            begin
              //Задаём цвет
              Stroke.Color := FPoint[j].Color;
              //Рисуем с учетом расстояния между точками
              DrawLine(FPoint[i].Point, FPoint[j].Point, ((100 / Me) * Abs(Me - D)) / 100);
            end;
          end;
        end;
      end;
      //А это у нас сами точки - объекты
      //Определяем размеры
      RectPoint := TRectF.Create(FPoint[i].Point, FPoint[i].Size, FPoint[i].Size);
      //Смещаем к центру элипса
      RectPoint.Offset(-FPoint[i].Size / 2, -FPoint[i].Size / 2);
      //Рисуем
      FillEllipse(RectPoint, 0.6);
    end;

    EndScene;
  end;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  //Сохраним размеры окна для использовании в потоке
  FScreenBounds := ClientRect;
  //Удаляем те, что вне видимости
  while FPoint.Count > (ClientWidth * ClientHeight) div 3500 do
  begin
    FPoint.Delete(RandomRange(1, FPoint.Count - 1));
  end;
  //Добавляем, если не хватает
  while FPoint.Count < (ClientWidth * ClientHeight) div 3500 do
  begin
    FPoint.Add(CreateRandomItem);
  end;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  Refill;
end;

end.

