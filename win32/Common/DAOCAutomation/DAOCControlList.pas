unit DAOCControlList;

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
  Classes, DAOCConnection, DAOCConnectionList, DAOCControl, MapNavigator;

type
  TDAOCControlList = class(TDAOCConnectionList)
  private
  protected
    function NewDAOCConnectionNeeded : TDAOCConnection; override;
  public
  end;
  
implementation

{ TDAOCControlList }

function TDAOCControlList.NewDAOCConnectionNeeded: TDAOCConnection;
begin
  Result := TDAOCControl.Create;
end;

end.
