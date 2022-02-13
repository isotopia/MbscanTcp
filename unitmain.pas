unit UnitMain;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
     StrUtils, unitabout,IniFiles,


  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids,
  ExtCtrls, Menus, ComCtrls, ActnList, mbubase, mbutcp, mbustdq,
  SNTPsend ,Crt, Types,blogger;    // mbuserial, ?? compile pas



// blogger, ,telnetsshclient

const
  Prog_Version_Major 	= 0.01;
  Prog_Version_Minor 	= 3;

type

  { TFormMBScanTcp }

  TFormMBScanTcp = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    ButtontestOn: TButton;
    ButtonSntp: TButton;
    ButtonSnmp: TButton;
    ButtonLoad: TButton;
    ButtonSave: TButton;
    ButtonDisconect: TButton;
    ButtonConnect: TButton;
    ButtontestOff: TButton;
    ButtontestPos: TButton;
    CheckBoxLogging: TCheckBox;
    CheckBox0base: TCheckBox;
    CheckBoxwriteEnable: TCheckBox;
    EditISlave: TEdit;
    EditStartReg: TEdit;
    EditRegCnt: TEdit;
    EditPort: TEdit;
    EditIP: TEdit;
    LabelLoopCount: TLabel;
    LabelUpdateRate: TLabel;
    LabelIP: TLabel;
    LabelIP1: TLabel;
    LabelIP2: TLabel;
    LabelIP5: TLabel;
    LabelStartReg: TLabel;
    LabelIP4: TLabel;
    MainMenu1: TMainMenu;
    MemoDebug: TMemo;
    MenuItemQuit: TMenuItem;
    MenuItemClearDescription: TMenuItem;
    MenuItemResetOfset: TMenuItem;
    MenuItemResetLoopCount: TMenuItem;
    MenuItemClearDebug: TMenuItem;
    MenuItemFile: TMenuItem;
    MenuItemLoadMBS: TMenuItem;
    MenuItemEdit: TMenuItem;
    MenuItemSaveMBS: TMenuItem;
    MenuItemAbout: TMenuItem;
    OpenDialog1: TOpenDialog;
    Splitter1: TSplitter;
    StringGrid: TStringGrid;
    Timer1: TTimer;
    TrackBarRate: TTrackBar;

    procedure Button1Click(Sender: TObject);
    procedure ButtonSntpClick(Sender: TObject);
    procedure ButtonLoadClick(Sender: TObject);
    procedure ButtontestOffClick(Sender: TObject);
    procedure ButtontestOnClick(Sender: TObject);
    procedure ButtonConnectClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure ButtonDisconectClick(Sender: TObject);
    procedure ButtontestPosClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MemoDebugChange(Sender: TObject);
    procedure MenuItemClearDescriptionClick(Sender: TObject);
    procedure MenuItemResetLoopCountClick(Sender: TObject);
    procedure MenuItemResetOfsetClick(Sender: TObject);
    procedure MenuItemClearDebugClick(Sender: TObject);
    procedure MenuItemLoadPrefClick(Sender: TObject);
    procedure MenuItemQuitClick(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure StringGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
     procedure StringGridGetEditText(Sender: TObject; ACol, ARow: Integer;
      var Value: string);
    procedure StringGridKeyPress(Sender: TObject; var Key: char);
    procedure Timer1Timer(Sender: TObject);
    procedure TrackBarRateChange(Sender: TObject);

    procedure OnReadHoldingRegisterDone(AQuery: TMBQuery);
    procedure OnWriteRegisterDone(AQuery: TMBQuery);
  private

  public
  mbt: TMBTCPClient;
  AQuery : TMBQuery;
  end;


var


  FormMBScanTcp: TFormMBScanTcp;



 // MbTcpRunSync : boolean;
  loopcount : integer;
  lastwritecol , lastwriterow :integer ;
 // FHeaderClicked: boolean;

  tfOut: TextFile;       // for saving data to file
  FileName : string;



const
  IniFileName = 'MbScanTcp.mbs';

implementation

{$R *.lfm}




{ TFormMBScanTcp }
procedure test1();
begin
     FormMBScanTcp.memoDebug.Lines.Add(  'test test1 ');
     FormMBScanTcp.memoDebug.Lines.Add(  ' event get query done called ') ;
end;


Procedure IniSaveState(filename : string );
var
  INI: TINIFile;
   j : integer;
begin
  // Create the object, specifying the the ini file that contains the settings
  INI := TINIFile.Create(IniFileName);

  try
   ini.WriteString('MbTcp'  ,'Slave'               ,FormMBScanTcp.EditISlave.Text);
   ini.WriteString('MbTcp'  ,'EditStartReg'         ,FormMBScanTcp.EditStartReg.Text);
   ini.WriteString('MbTcp'  ,'EditRegCnt'           ,FormMBScanTcp.EditRegCnt.Text);
   ini.WriteString('MbTcp'  ,'EditIP'               ,FormMBScanTcp.EditIP.Text);
   ini.WriteString('MbTcp'  ,'EditPort'             ,FormMBScanTcp.EditPort.Text);
   ini.WriteBool  ('MbTcp'  ,'CheckBoxLoging'       ,FormMBScanTcp.CheckBoxLogging.Checked);

   for j := 0 to FormMBScanTcp.stringgrid.ColCount -1  do
     begin
     ini.WriteInteger('ColWidth'  ,inttostr(j) , FormMBScanTcp.StringGrid.ColWidths[j] );
     end;

   for j := 1 to FormMBScanTcp.stringgrid.RowCount -1  do
     begin
     ini.WriteString('Ofset +x'  ,inttostr(j) ,FormMBScanTcp.StringGrid.Cells[5, j ] );
     end;

   for j := 1 to FormMBScanTcp.stringgrid.RowCount -1  do
     begin
     ini.WriteString('Multiplier *x'  ,inttostr(j) ,FormMBScanTcp.StringGrid.Cells[6, j ] );
     end;

   for j := 1 to FormMBScanTcp.stringgrid.RowCount -1  do
     begin
     ini.WriteString('Logging Data'  ,inttostr(j) ,FormMBScanTcp.StringGrid.Cells[7, j ] );
     end;

     for j := 1 to FormMBScanTcp.stringgrid.RowCount -1  do
     begin
     ini.WriteString('Description'  ,inttostr(j) ,FormMBScanTcp.StringGrid.Cells[8, j ] );
     end;

  finally
    // After the ini file was used it must be freed to prevent memory leaks.
    INI.Free;
  end;
end;

Procedure IniLoadState();
var
  INI: TINIFile;
   j : integer;
begin

    INI := TINIFile.Create(IniFileName);

  try
  FormMBScanTcp.EditISlave.Text         := ini.ReadString('MbTcp'   ,'Slave'               ,'1');
  FormMBScanTcp.EditStartReg.Text       := ini.ReadString('MbTcp'  ,'EditStartReg'         ,'1');
  FormMBScanTcp.EditRegCnt.Text         := ini.ReadString('MbTcp'  ,'EditRegCnt'           ,'10');
  FormMBScanTcp.EditIP.Text             := ini.ReadString('MbTcp'  ,'EditIP'               ,'192.168.0.10');
  FormMBScanTcp.EditPort.Text           := ini.ReadString('MbTcp'  ,'EditPort'             ,'502');
  FormMBScanTcp.CheckBoxLogging.Checked := ini.ReadBool('MbTcp'  ,'CheckBoxLoging'       ,true);

   for j := 0 to FormMBScanTcp.stringgrid.ColCount -1  do
     begin
     FormMBScanTcp.StringGrid.ColWidths[j] := ini.ReadInteger('ColWidth'  ,inttostr(j) , 70 );
     end;

   for j := 1 to FormMBScanTcp.stringgrid.RowCount -1  do
     begin
     FormMBScanTcp.StringGrid.Cells[5, j ] := ini.ReadString('Ofset +x'  ,inttostr(j) , '0' );
     end;

   for j := 1 to FormMBScanTcp.stringgrid.RowCount -1  do
     begin
     FormMBScanTcp.StringGrid.Cells[6, j ] := ini.ReadString('Multiplier *x'  ,inttostr(j) , '1');
     end;

{   for j := 1 to FormMBScanTcp.stringgrid.RowCount -1  do
     begin
     FormMBScanTcp.StringGrid.Cells[7, j ] := ini.ReadString('Logging Data'  ,inttostr(j) , '' );
     end;       }

   for j := 1 to FormMBScanTcp.stringgrid.RowCount -1  do
     begin
     FormMBScanTcp.StringGrid.Cells[8, j ] := ini.ReadString('Description'  ,inttostr(j) , '');
     end;




  finally
    // After the ini file was used it must be freed to prevent memory leaks.
    INI.Free;
  end;

  end;

procedure  TFormMBScanTcp.OnReadHoldingRegisterDone(AQuery: TMBQuery);
var
   j : integer;
   q : TMBQCustomRWRegisters;
   sr :  uint32 ;
   MaskLW :LongWord;
   CsvDataList  : string;

begin
   q := AQuery as TMBQCustomRWRegisters;
   sr := strtointdef(EditStartReg.Caption , 1);

   q.WaitForCompletion;         //-----------------------------    cool , ca marche

//   q.WaitForCompletion;

    FormMBScanTcp.memoDebug.Lines.Add(  ' ---'  );
   if q.Error <> mbeNoError then
       memoDebug.Lines.Add ( 'read error  : ' +  mbu_ErrorToStr(q.Error) + '     LoopCount ' + inttostr(Loopcount)  )
   else
   begin
      lastwritecol :=0;
      lastwriterow := 0;
   end;


 // memoDebug.Lines.Add(  ' q.rp.ByteCount '+ inttostr( q.rp.ByteCount) );

   CsvDataList := FormatDateTime('YYYY-MM-DD HH:NN:SS',now) + '  ';


   for j := 0 to (q.rp.ByteCount div 2) -1  do     // -de -1 a - 3 temporaire debug int to float
   begin
   //  memoDebug.Lines.Add('recu de q.rp  '+ inttostr(j) + '  '  + inttostr( q.rp.RegU16BE[j] ));

   StringGrid.RowCount := q.rq.RegCnt + 1 ;

   StringGrid.Cells[0, j +1 ] :=  inttostr(j+ sr );              // curent register
   StringGrid.Cells[1, j +1 ] :=  inttostr(smallint( q.rp.RegU16BE[j]) );         // col integer
   StringGrid.Cells[2, j +1 ] :=  inttohex( q.rp.RegU16BE[j] ,8);                // col hex
   StringGrid.Cells[3, j +1 ] :=  IntToBin( q.rp.RegU16BE[j] ,16);               // col bin

       if j  mod 2 = 1 then   // if j is odd , second integer - routine for 32bit assembly
       begin
        //StringGrid.Cells[4, j +1 ] :=  inttostr( q.rp.RegU16BE[j] );
         MaskLW  := q.rp.RegU16BE[j] shl 16  ;
         MaskLW  := MaskLW  or q.rp.RegU16BE[j-1];

         StringGrid.Cells[4, j +1 ] :=  inttostr( MaskLW   );
         StringGrid.Cells[4, j  ]   :=  inttohex( MaskLW ,8);
       end ;

     if  StringGrid.Cells[5, j +1 ] = '' then                                       // 'Ofset +x';
                         StringGrid.Cells[5, j +1 ] := '0';

     if  StringGrid.Cells[6, j +1 ] = '' then                                      // 'Multiplier *x'
                         StringGrid.Cells[6, j +1 ] := '1';

                                                                          // 'Logging Data' mx+b scale plus ofset ;
      StringGrid.Cells[7, j+1 ] := floattostr( (smallint( q.rp.RegU16BE[j]) + strtofloat(StringGrid.Cells[5, j +1 ])) * strtofloat(StringGrid.Cells[6, j +1 ]) );

      CsvDataList := CsvDataList + '  , ' + StringGrid.Cells[7, j+1 ]  ;     // logging data to file  scaled

   end;
   loopcount := loopcount +1;
   LabelLoopCount.Caption:= ('loopcount =  '+ inttostr(Loopcount) );

 q.free;

 mbt.GetCompletedQuery;  // reset query list , hang after 1024 pool if not there,

  //---------------------- save pool dada to file   with CsvDataLis

   AssignFile(tfOut, FileName);  // save table data to file.
    // Use exceptions to catch errors (this is the default so not absolutely requried) // {$I+}
    try
      Append(tfout) ;  // append to file
      writeln(tfOut, CsvDataList);
      CloseFile(tfOut);
    except
      // If there was an error the reason can be found here
      on E: EInOutError do
       begin
       memoDebug.Lines.Add  ('write File handling error occurred. Details: ' + E.ClassName +E.Message );
       end;
    end;

 end;



procedure TFormMBScanTcp.ButtonConnectClick(Sender: TObject);
var
  CsvDataList  : string;
  j : integer;
begin


  if  timer1.Enabled = true then ButtonDisconect.Click ;


     CsvDataList := 'Date and Time ,';
     for j := 1 to FormMBScanTcp.stringgrid.RowCount -1  do
     begin
     CsvDataList := CsvDataList + FormMBScanTcp.StringGrid.Cells[8, j ] + ' , ' ;
     end;

  FileName :=  FormatDateTime('YYYY-MM-DD-HH-NN-SS',now) +'.csv' ;
    AssignFile(tfOut, FileName);
    try       // Create the file,.
      rewrite(tfOut);      // Create the file, write some text and close it.
      writeln(tfOut, CsvDataList);
      CloseFile(tfOut);

    except
      // If there was an error the reason can be found here
      on E: EInOutError do
      begin
       memoDebug.Lines.Add  ('File handling error occurred. Details: ' + E.ClassName +E.Message );
      end;
    end;
     //-------------------------- modbus tcp stuff


  mbt := TMBTCPClient.Create;
  mbt.TargetHost := editip.Text;    // '172.21.0.200';
  mbt.TargetPort := editport.Text;  //'502';
  mbt.Connect ;    // bug return connected even while not !!

  timer1.Enabled:= true ;
  timer1.Tag := 1;
  //memoDebug.Lines.Add(  'fin de ButtonConnectClick');


end;




procedure TFormMBScanTcp.ButtonSntpClick(Sender: TObject);
var
  sntp:TSntpSend;
begin
 {
 some server avialable
 time.google.com
 time.nist.gov
}
  sntp:=TSntpSend.Create;
  try
    sntp.TargetHost:= 'time.google.com';
    if sntp.GetSNTP
      then memoDebug.Lines.Add( Datetimetostr(sntp.NTPTime)+' UTC' )
      else memoDebug.Lines.Add('SNTP Not contacted!');
  finally
    sntp.Free;
  end;
end;

procedure TFormMBScanTcp.Button1Click(Sender: TObject);
begin
  memoDebug.Lines.Add('button 1 clic ');
 // TQueryApp.cmdl.Add('1254'); // FCommands;
end;



procedure TFormMBScanTcp.ButtonSaveClick(Sender: TObject);
begin
  IniSaveState('tata');
end;

procedure TFormMBScanTcp.ButtonLoadClick(Sender: TObject);
//var
//  INI: TINIFile;
 // j : integer;
begin
  // Create the object, specifying the the ini file that contains the settings

   If FileExists(IniFileName) Then
      begin
       memoDebug.Lines.Add('MbScanTcp.mbs exist ,Loading Preferance');
       IniLoadState();
      end
   else
   begin
      memoDebug.Lines.Add('MbScanTcp.mbs not found , Creating one');
      IniSaveState('tas');
   end;

end;

procedure TFormMBScanTcp.ButtontestPosClick(Sender: TObject);
var
   q : TMBQCustomRWRegisters;
begin
            // -------------- test purpose only ---------------
  q := TMBQWriteHoldingRegisters.Create(1);
  q.rq.StartReg := strtoint('1547');//

  q.rq.RegCnt := strtoint('2'); //   2 register;
  q.rq.RegU16BE[0]  := random(10000) ;      // first is LSB = 1 start
  q.rq.RegU16BE[1]  := 0 ;
  //q.rp
  mbt.SubmitQuery(q);
  q.WaitForCompletion;

    if q.Error <> mbeNoError then
    memoDebug.Lines.Add ( 'write error  : ' +  mbu_ErrorToStr(q.Error) );

  q.free;
end;


procedure TFormMBScanTcp.ButtontestOffClick(Sender: TObject);
var
   q : TMBQCustomRWRegisters;
begin
          // -------------- test purpose only ---------------
  q := TMBQWriteHoldingRegisters.Create(1);
  q.rq.StartReg := strtoint('573');//     573 = p2.030 , 1 start drive     1313 curent encoder pos

  q.rq.RegCnt := strtoint('2'); //   2 register;
  q.rq.RegU16BE[0]  := 0 ;      // first is LSB = 1 start
  q.rq.RegU16BE[1]  := 0 ;

  mbt.SubmitQuery(q);
  q.WaitForCompletion;

    if q.Error <> mbeNoError then
    memoDebug.Lines.Add ( 'write error  : ' +  mbu_ErrorToStr(q.Error) );


  q.free;


end;

procedure  TFormMBScanTcp.OnWriteRegisterDone(AQuery: TMBQuery);
var
   q : TMBQCustomRWRegisters;
begin
  q := AQuery as TMBQCustomRWRegisters;
  if q.Error <> mbeNoError then
    memoDebug.Lines.Add ( 'write error  : ' +  mbu_ErrorToStr(q.Error) );
  q.free;
end;

procedure TFormMBScanTcp.ButtontestOnClick(Sender: TObject);
var
   q : TMBQCustomRWRegisters;
begin
         // -------------- test purpose only ---------------
  q := TMBQWriteHoldingRegisters.Create(1);
  q.rq.StartReg := strtoint('573');//     573 = p2.030 , 1 start drive     1313 curent encoder pos

  q.rq.RegCnt := strtoint('2'); //   2 register;
  q.rq.RegU16BE[0]  := 1 ;      // first is LSB = 1 start
  q.rq.RegU16BE[1]  := 0 ;
  q.OnRxDone:= @OnWriteRegisterDone;

  mbt.SubmitQuery(q);
{  q.WaitForCompletion;

    if q.Error <> mbeNoError then
    memoDebug.Lines.Add ( 'write error  : ' +  mbu_ErrorToStr(q.Error) );


  q.free;
 }
end;


procedure TFormMBScanTcp.ButtonDisconectClick(Sender: TObject);
begin

  timer1.Enabled:= false ;


   if timer1.tag = 1 then
     begin
      timer1.Tag := 0;   // tag used for remember if connected requested
                         //zz memoDebug.Lines.Add(  ' on part pour les delay ');
    //  Delay(100); {Wait one second}
    //  application.ProcessMessages;
       //     Delay(200); {Wait one second}
                        //zz  memoDebug.Lines.Add(  ' fin des delay ');
       mbt.Disconnect;
                        //zz   memoDebug.Lines.Add(  ' apres get completed query ') ;
       //mbt.Free;    there is a leak in mbwrite , after 1 write , could not free mbt     ---------------------------- to check cf
      end;

  memoDebug.Lines.Add(  'Disconnected  ');
end;




procedure TFormMBScanTcp.FormCreate(Sender: TObject);
begin

  stringGrid.ColCount := 10;
  StringGrid.Cells[0, 0] := 'Register';
  StringGrid.Cells[1, 0] := 'int';
  StringGrid.Cells[2, 0] := 'hex';
  StringGrid.Cells[3, 0] := 'Bin';
  StringGrid.Cells[4, 0] := 'Hex and dec 32 bit dual reg';
  StringGrid.Cells[5, 0] := 'Ofset +x';
  StringGrid.Cells[6, 0] := 'Multiplier *x';
  StringGrid.Cells[7, 0] := 'Logging Data';
  StringGrid.Cells[8, 0] := 'Description (Saved on close)';

  stringgrid.ColWidths[0]:= 60;
  loopcount := 0;

  IniLoadState();

end;

procedure TFormMBScanTcp.FormDestroy(Sender: TObject);
begin
  IniSaveState('gg');
  ButtonDisconect.Click  ;
end;

procedure TFormMBScanTcp.MemoDebugChange(Sender: TObject);
begin

end;

procedure TFormMBScanTcp.MenuItemClearDescriptionClick(Sender: TObject);
var
j : integer ;
begin

   for j := 1 to FormMBScanTcp.stringgrid.RowCount -1  do
   begin
   FormMBScanTcp.StringGrid.Cells[8, j ] :=  ' ' ;
   end;
end;

procedure TFormMBScanTcp.MenuItemResetLoopCountClick(Sender: TObject);
begin
  loopcount := 0;
end;

procedure TFormMBScanTcp.MenuItemResetOfsetClick(Sender: TObject);
var
  j : integer ;
begin

     for j := 1 to FormMBScanTcp.stringgrid.RowCount -1  do
     begin
     FormMBScanTcp.StringGrid.Cells[5, j ] :=  '0' ;
     end;

   for j := 1 to FormMBScanTcp.stringgrid.RowCount -1  do
     begin
     FormMBScanTcp.StringGrid.Cells[6, j ] :=  '1' ;
     end;

end;

procedure TFormMBScanTcp.MenuItemClearDebugClick(Sender: TObject);
begin
  MemoDebug.Clear;
end;

procedure TFormMBScanTcp.MenuItemLoadPrefClick(Sender: TObject);
  var
  filename: string;
  begin

    if OpenDialog1.Execute then
        begin
        filename := OpenDialog1.Filename;
        ShowMessage(filename);
        end;

  end;


procedure TFormMBScanTcp.MenuItemQuitClick(Sender: TObject);
begin
  application.Terminate;
end;

procedure TFormMBScanTcp.MenuItemAboutClick(Sender: TObject);
begin
 formabout.Show;
 end;


procedure TFormMBScanTcp.StringGridDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
begin

    if  ((acol = 2) or (acol = 1)) and ( timer1.Tag = 1) then
    begin
         if (lastwritecol = aCol) and (lastwriterow = arow) and (lastwritecol <>0) and (lastwriterow <>0) then
         begin
              StringGrid.Canvas.Brush.Color := clRed;
              stringgrid.canvas.FillRect(arect);
         end
    end;
end;



procedure TFormMBScanTcp.StringGridGetEditText(Sender: TObject; ACol,
  ARow: Integer; var Value: string);
begin
   // memoDebug.Lines.Add ( 'getedit text');
   if (timer1.Tag = 1 )and ((StringGrid.Col = 1 ) or (StringGrid.Col = 2 ) or(StringGrid.Col = 3 )) then
   begin
      timer1.Enabled:= false ;
   end;
end;

procedure TFormMBScanTcp.StringGridKeyPress(Sender: TObject; var Key: char);
  var
  a : integer ;
   q : TMBQCustomRWRegisters;
  //BinaryStream: TBytesStream;
 // bytes: TBytes;
 // HexStr: String;
  begin

     //zz  memoDebug.Lines.Add ( 'StringGridKeyPress ');
      a := 0 ;

      if StringGrid.Col = 1 then
      begin
       a :=  strtointdef(Stringgrid.Cells[  StringGrid.Col , StringGrid.Row ],0 ) ;
      end;

      if StringGrid.Col = 2 then
      begin
        a :=  Hex2Dec(Stringgrid.Cells[  StringGrid.Col , StringGrid.Row ] ) ;
        // memoDebug.Lines.Add ( 'debug hex   : ' +  inttostr(a) );
      end;

      if StringGrid.Col = 3 then
      begin

         memoDebug.Lines.Add ( 'not supported' );
      end;

        if (  (key = chr(13)) and ((timer1.Tag = 1 ) and ((StringGrid.Col = 1 ) or (StringGrid.Col = 2 ) or(StringGrid.Col = 3 )))) then   // write only if modbus connected
          begin
           if CheckBoxwriteEnable.Checked then
           begin
            q := TMBQWriteHoldingRegisters.Create(byte(1));
            q.rq.StartReg :=  StringGrid.Row + strtoint(EditStartReg.Caption) - 1;
            q.rq.RegCnt := strtoint('1');
            q.rq.RegU16BE[0]  := a;
             // q.rq.RegU16BE[i] := StrToInt(cmdl.Arguments[FArgCnt + i + 2]);

             mbt.SubmitQuery(q);
             q.WaitForCompletion;

             if q.Error <> mbeNoError then
             begin
                  memoDebug.Lines.Add ( 'write error  : ' +  mbu_ErrorToStr(q.Error) )
             end
             else
             begin
               memoDebug.Lines.Add ( 'value Writen  : ' +  inttostr(a) );
               lastwritecol := StringGrid.Col;
               lastwriterow := StringGrid.Row;
             end;


          q.free;   //----------------------------------------------------:(
          end ;
          if timer1.Tag = 1 then      // parfois le tag a ete cjanger entre temps
             timer1.Enabled := true ;
          end;
end;


procedure TFormMBScanTcp.Timer1Timer (Sender: TObject);
var
  q: TMBQCustomRWRegisters ; //TMBQuery;
begin

  mbt.ReplyTimeout:=1000;      // test

  q := TMBQReadHoldingRegisters.Create(1);

  q.rq.StartReg := strtointdef(EditStartReg.Caption, 1);//     1837;
  q.rq.RegCnt := strtointdef(Editregcnt.Caption, 10); //   10;
  q.OnRxDone := @OnReadHoldingRegisterDone;
  mbt.SubmitQuery(q);


end;


procedure TFormMBScanTcp.TrackBarRateChange(Sender: TObject);
begin
  timer1.Interval:= TrackBarRate.Position;
  labelUpdateRate.Caption:= inttostr(TrackBarRate.Position) +'  ms';
end;


end.

