program FixBiolife;

 {Converts Delphi biolife.xml to stripped-down nested-element biolife.fixed
   and saved images to external .bmp files.
  Resulting files can be used with fishfacts_example ExtPascal app.
  
  Note that this program requires Free Pascal to compile.
  
  To deploy resulting files:
   - Change .gif to .bmp in mainform.pas.
   - Rename biolife.fixed to biolife.xml.}

{$MODE DELPHI}

uses
  SysUtils,
  Classes,
  base64,
  XMLRead,   {FPC XML reading routines}
  XMLWrite,  {FPC XML writing routines}
  DOM;       {FPC DOM interfaces}

var
  XmlFName      : AnsiString;
  XmlDoc1       : TXMLDocument;
  RowDataNode   : TDOMNode;
  RowIndex      : Integer;
  RowNode       : TDOMNode;
  RowAttrs      : TDOMNamedNodeMap;
  AttrNode      : TDOMNode;
  AttrText      : AnsiString;
  BmpFileName   : AnsiString;
  GraphicStream : TMemoryStream;
  Base64Decoder : TBase64DecodingStream;
  ByteCnt       : Integer;
  FileStream    : TFileStream;
  XmlDoc2       : TXMLDocument;
  RootNode      : TDOMNode;
  RowNode2      : TDOMNode;
  DataNode      : TDOMNode;
  LengthIn      : Double;
begin
  XmlFName := 'biolife.xml';
  if not FileExists(XmlFName) then
    begin
    WriteLn('Can''t find ', XmlFName);
    Exit;
    end;
    
  XmlRead.ReadXmlFile(XmlDoc1, XmlFName);

  if XmlDoc1.DocumentElement.NodeName <> 'DATAPACKET' then
    begin
    WriteLn('Can''t find DATAPACKET root element.');
    Exit;
    end;

  RowDataNode := XmlDoc1.DocumentElement.FindNode('ROWDATA');
  if not Assigned(RowDataNode) then
    begin
    WriteLn('Can''t find ROWDATA element.');
    Exit;
    end;
    
   {Convert and save graphic images as .bmp files}
  for RowIndex := 0 to RowDataNode.ChildNodes.Count-1 do
    begin
    RowNode := RowDataNode.ChildNodes.Item[RowIndex];
    RowAttrs := RowNode.Attributes;

    AttrNode := RowAttrs.GetNamedItem('Common_Name');
    if Assigned(AttrNode) then  {Has attribute with that field name?}
      begin
      AttrNode := AttrNode.FindNode('#text');
      BmpFileName := Trim(AttrNode.NodeValue) + '.bmp';
      end
    else
      Continue;

    AttrNode := RowAttrs.GetNamedItem('Graphic');
    if Assigned(AttrNode) then  {Has attribute with that field name?}
      begin
      AttrNode := AttrNode.FindNode('#text');
      if Assigned(AttrNode) then
        begin
        AttrText := Trim(AttrNode.NodeValue);  //Convert from WideString
        GraphicStream := TMemoryStream.Create;
        GraphicStream.Write(AttrText[1], Length(AttrText)); 
        GraphicStream.Position := 0;
        Base64Decoder := TBase64DecodingStream.Create(GraphicStream, bdmMIME);
        for ByteCnt := 1 to 8 do  {Skip down to the BMP file data}
          Base64Decoder.ReadByte;
        FileStream := TFileStream.Create(BmpFileName, fmCreate);
        FileStream.CopyFrom(Base64Decoder, Base64Decoder.Size-8);
        FileStream.Free;
        GraphicStream.Free;
        Base64Decoder.Free;
        end;
      end;
    end;
    
   {Convert attribute data to nested element data, with minimal
     error checking}
  XmlDoc2 := TXMLDocument.Create;
  RootNode := XmlDoc2.CreateElement('DATAPACKET');
  XmlDoc2.AppendChild(RootNode);  {Root element}
  for RowIndex := 0 to RowDataNode.ChildNodes.Count-1 do
    begin
    RowNode := RowDataNode.ChildNodes.Item[RowIndex];
    RowAttrs := RowNode.Attributes;
    RowNode2 := RootNode.OwnerDocument.CreateElement('ROW');
    RootNode.AppendChild(RowNode2);

    AttrNode := RowAttrs.GetNamedItem('Species_No');
    AttrNode := AttrNode.FindNode('#text');
    DataNode := RowNode2.OwnerDocument.CreateElement('Species_No');
    DataNode.AppendChild(RowNode2.OwnerDocument.CreateTextNode(AttrNode.NodeValue));
    RowNode2.AppendChild(DataNode);

    AttrNode := RowAttrs.GetNamedItem('Category');
    AttrNode := AttrNode.FindNode('#text');
    DataNode := RowNode2.OwnerDocument.CreateElement('Category');
    DataNode.AppendChild(RowNode2.OwnerDocument.CreateTextNode(AttrNode.NodeValue));
    RowNode2.AppendChild(DataNode);

    AttrNode := RowAttrs.GetNamedItem('Common_Name');
    AttrNode := AttrNode.FindNode('#text');
    DataNode := RowNode2.OwnerDocument.CreateElement('Common_Name');
    DataNode.AppendChild(RowNode2.OwnerDocument.CreateTextNode(AttrNode.NodeValue));
    RowNode2.AppendChild(DataNode);

    AttrNode := RowAttrs.GetNamedItem('Species_Name');
    AttrNode := AttrNode.FindNode('#text');
    DataNode := RowNode2.OwnerDocument.CreateElement('Species_Name');
    DataNode.AppendChild(RowNode2.OwnerDocument.CreateTextNode(AttrNode.NodeValue));
    RowNode2.AppendChild(DataNode);

    AttrNode := RowAttrs.GetNamedItem('Length__cm_');
    AttrNode := AttrNode.FindNode('#text');
    DataNode := RowNode2.OwnerDocument.CreateElement('Length_Cm');
    DataNode.AppendChild(RowNode2.OwnerDocument.CreateTextNode(AttrNode.NodeValue));
    RowNode2.AppendChild(DataNode);

    AttrNode := RowAttrs.GetNamedItem('Length_In');
    AttrNode := AttrNode.FindNode('#text');
    DataNode := RowNode2.OwnerDocument.CreateElement('Length_In');
    LengthIn := StrToFloat(AttrNode.NodeValue);
    DataNode.AppendChild(RowNode2.OwnerDocument.CreateTextNode(
     FloatToStrF(LengthIn, ffGeneral, 0, 0)));  //Get rid of useless decimal places.
    RowNode2.AppendChild(DataNode);

    AttrNode := RowAttrs.GetNamedItem('Notes');
    AttrNode := AttrNode.FindNode('#text');
    DataNode := RowNode2.OwnerDocument.CreateElement('Notes');
    DataNode.AppendChild(RowNode2.OwnerDocument.CreateTextNode(AttrNode.NodeValue));
    RowNode2.AppendChild(DataNode);

    end;

  WriteXMLFile(XmlDoc2, ChangeFileExt(XmlFName, '.fixed'));
  XmlDoc1.Free;
  XmlDoc2.Free;
end.


