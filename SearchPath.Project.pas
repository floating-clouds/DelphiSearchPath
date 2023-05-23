unit SearchPath.Project;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  ToolsAPI, Semin64.Xml, SearchPath.XMLDocument, SearchPath.Common;

type
  TProjectLogMessageEvent = procedure (Sender: TObject; Text: string; LogLevel: TLogLevel) of object;

  TProject = class;

  TProjectMapping = class
  private
    FProject: TProject;
    FDelphiProject: IOTAProject;
  public
    constructor Create(AProject: TProject; ADelphiProject: IOTAProject); overload;
    destructor Destroy; override;

    property Project: TProject read FProject;
    property DelphiProject: IOTAProject read FDelphiProject;
  end;

  TDocumentBase = class
  strict private
    FCol: Integer;
    FRow: Integer;
    FDoc: TXMLDocumentObject;
    FElement: PsanXMLObjElement;
    FProject: TProject;
    procedure SetLocation(AElement: PsanXMLObjElement);
  protected
    function GetAttributeValue(const AttrName: string): string;
    function GetElement(AElementName: string): PsanXMLObjElement; overload;
    function GetElement(AElementIndex: Integer): PsanXMLObjElement; overload;
    function GetChildElementValue(AElementName: string): string;
    function ChildCount: Integer;
    function GetName: string;
    function GetValue: string;
    function iif<T>(B: Boolean; T1, T2: T): T;
  public
    constructor Create(AProject: TProject; AElement: PsanXMLObjElement); overload;

    procedure Debug(Text: string); overload;
    procedure Info(Text: string); overload;
    procedure Warn(Text: string); overload;
    procedure Error(Text: string); overload;
    procedure Debug(const Format: string; const Args: array of const); overload;
    procedure Info(const Format: string; const Args: array of const); overload;
    procedure Warn(const Format: string; const Args: array of const); overload;
    procedure Error(const Format: string; const Args: array of const); overload;

    property Project: TProject read FProject;
    property Row: Integer read FRow;
    property Col: Integer read FCol;
  end;

  TProxyFilter = class(TDocumentBase)
  strict private
    FFilter: string;
  public
    destructor Destroy; override;
    procedure Parse;

    property Filter: string read FFilter;
  end;

  TProxyFilters = class(TDocumentBase)
  strict private
    FFilters: TList<TProxyFilter>;
  public
    destructor Destroy; override;
    procedure Parse;
    property Filters: TList<TProxyFilter> read FFilters;
  end;

  TProxy = class(TDocumentBase)
  strict private
    FHost: string;
    FPort: Integer;
    FUser: string;
    FPassword: string;
    FEnabled: Boolean;
    FExcludeFilters: TProxyFilters;
    FIncludeFilters: TProxyFilters;
  public
    destructor Destroy; override;
    procedure Parse;

    property Host: string read FHost;
    property Port: Integer read FPort;
    property User: string read FUser write FUser;
    property Password: string read FPassword write FPassword;
    property Enabled: Boolean read FEnabled;
    property IncludeFilters: TProxyFilters read FIncludeFilters;
    property ExcludeFilters: TProxyFilters read FExcludeFilters;
  end;

  TProperty = class(TDocumentBase)
  strict private
    FName: string;
    FValue: string;
  public
    procedure Parse;
    procedure AddSystemProperty(AName, AValue: string);
    procedure AddSpecialProperty(AName, AValue: string);
    function ToString: string; override;

    property Name: string read FName;
    property Value: string read FValue;
  end;

  TProperties = class(TDocumentBase)
  const
    DUP_MESSAGE = 'Duplicates %s property: %s, skipped %s';
  strict private
    FProperties: TStrings;
    procedure ResolvePropertyValues;
    procedure AddCustomProperties;
    procedure AddSpecialProperties;
    procedure AddSystemProperties;
  public
    destructor Destroy; override;
    procedure Parse(ExistsCustomeProperties: Boolean);
    function GetValue(const AName: string): string;
    property Properties: TStrings read FProperties;
  end;

  TLibPath = class(TDocumentBase)
  strict private
    FPath: string;
  public
    procedure Parse(PathPrefix: string);
    property Path: string read FPath;
  end;

  TLibPaths = class(TDocumentBase)
  strict private
    FPaths: TStrings;
  public
    destructor Destroy; override;
    procedure Parse(const PathPrefix: string);

    function GetSearchPath(Strings: TStrings): string;

    property Paths: TStrings read FPaths;
  end;

  TDependencie = class(TDocumentBase)
  strict private
    FGroup: string;
    FVersion: string;
    FArtifactId: string;
    FCompileVersion: string;
    FType: string;
    FPath: string;
    FName: string;
    FLibPaths: TLibPaths;
    FRepo: string;
    procedure Unzip(const ZipFile, FilesRoot: string);
  public
    destructor Destroy; override;
    procedure Parse;

    procedure GetSearchPath(Strings: TStrings);
    procedure DownloadPackage;

    property Group: string read FGroup;
    property ArtifactId: string read FArtifactId;
    property Version: string read FVersion;
    property CompileVersion: string read FCompileVersion;
    property &Type: string read FType;
    property LibPaths: TLibPaths read FLibPaths;
    property Repo: string read FRepo;
  end;

  TDependencies = class(TDocumentBase)
  strict private
    FDependencies: TList<TDependencie>;
  public
    destructor Destroy; override;
    procedure Parse;

    procedure GetSearchPath(Strings: TStrings);
    procedure DownloadPackage;
  end;

  TRepository = class(TDocumentBase)
  private
    FId: string;
    FName: string;
    FURL: string;
  public
    procedure Parse;

    property Id: string read FId write FId;
    property Name: string read FName write FName;
    property URL: string read FURL write FURL;
  end;

  TRepositories = class(TDocumentBase)
  strict private
    FRepositories: TList<TRepository>;
  public
    constructor Create(AProject: TProject; AElement: PsanXMLObjElement);
    destructor Destroy; override;
    procedure Parse;

    property Repositories: TList<TRepository> read FRepositories;
  end;

  TProject = class(TDocumentBase)
  private
    const REPOSITORIES_KEY = 'repositories';
    const PROXY_KEY        = 'proxy';
    const PROPERTIES_KEY   = 'properties';
    const DEPENDENCIES_KEY = 'dependencies';
  strict private
    FVer: string;
    FLogLevel: TLogLevel;
    FProperties: TProperties;
    FProxy: TProxy;
    FRepositories: TRepositories;
    FDependencies: TDependencies;
    FParent: TProject;
    FDocument: TXMLDocumentObject;
    FProjectName: string;
    FLogMessage: TProjectLogMessageEvent;
    function GetRepositories: TList<TRepository>;
    procedure ParseRepositories;
    procedure ParseDependencies;
    procedure ParseProperties;
    procedure ParseProxy;
  public
    constructor Create(AParent: TProject; AProjectName: string);
    destructor Destroy; override;
    procedure Parse;

    function GetPropertyValue(const AName: string): string;

    property Document: TXMLDocumentObject read FDocument;
    property ProjectName: string read FProjectName;
    // 文档中的内容
    property Ver: string read FVer;
    property LogLevel: TLogLevel read FLogLevel;
    property Proxy: TProxy read FProxy;
    property Repositories: TList<TRepository> read GetRepositories;
    property Properties: TProperties read FProperties;
    property Dependencies: TDependencies read FDependencies;
    //
    property LogMessage: TProjectLogMessageEvent read FLogMessage write FLogMessage;
  end;

  TProjects = class
  strict private
    FRootProject: TProject;
    FSearchPath: ISearchPathManager;
    FProjects: TDictionary<string, TProjectMapping>;
    procedure Init;
    procedure ParseProject(const AProjectName: string);
    procedure UpdateProjectLib(const AProjectName: string);
    procedure ApplySearchPath(const AProjectName: string);
    procedure ProjectLogMessage(Sender: TObject; Text: string; LogLevel: TLogLevel);
  public
    constructor Create(ASearchPathManager: ISearchPathManager);
    destructor Destroy; override;

    procedure Parse(const AProjectName: string = '');
    procedure UpdateProjectLibs(const AProjectName: string = '');
    procedure ApplyProjectSearchPath(const AProjectName: string = '');
  end;

implementation

uses
  System.IOUtils, System.Net.HttpClient, System.Zip,
  Vcl.Graphics,
  Winapi.Windows,
  SearchPath.Config;

{ TLocation }

procedure TDocumentBase.Debug(Text: string);
begin
  FProject.LogMessage(Self, Text, llDebug);
end;

procedure TDocumentBase.Error(Text: string);
begin
  FProject.LogMessage(Self, Text, llError);
end;

procedure TDocumentBase.Info(Text: string);
begin
  FProject.LogMessage(Self, Text, llInfo);
end;

procedure TDocumentBase.SetLocation(AElement: PsanXMLObjElement);
begin
  if AElement <> nil then begin
    FRow := PsanXMLObjElementEx2(AElement).LineNo;
    FCol := PsanXMLObjElementEx2(AElement).CharNoInLine;
  end else begin
    FRow := 0;
    FCol := 0;
  end;
end;

procedure TDocumentBase.Warn(const Format: string; const Args: array of const);
begin
  Warn(System.SysUtils.Format(Format, Args));
end;

procedure TDocumentBase.Warn(Text: string);
begin
  FProject.LogMessage(Self, Text, llWarn);
end;

constructor TProject.Create(AParent: TProject; AProjectName: string);
begin
  FProjectName := AProjectName;
  FParent := AParent;

  FDocument := TXMLDocumentObject.Create;
  FDocument.LoadFromFile(AProjectName);

  inherited Create(Self, FDocument.RootElement);
end;

destructor TProject.Destroy;
begin
  if FDependencies <> nil then FreeAndNil(FDependencies);
  if FRepositories <> nil then FreeAndNil(FRepositories);
  if FProperties <> nil then FreeAndNil(FProperties);
  if FProxy <> nil then FreeAndNil(FProxy);
  if FDocument <> nil then FreeAndNil(FDocument);

  inherited;
end;

function TProject.GetPropertyValue(const AName: string): string;
begin
  Result := FProperties.GetValue(AName);
end;

function TProject.GetRepositories: TList<TRepository>;
begin
  Result := FRepositories.Repositories;
end;

procedure TProject.Parse;
begin
  FVer := GetAttributeValue('ver');
  FLogLevel := TLogLevel.From(GetAttributeValue('logLevel'));

  ParseProxy;
  ParseProperties;
  ParseRepositories;
  ParseDependencies;
end;

procedure TProject.ParseDependencies;
begin
  var DependenciesElement := GetElement(DEPENDENCIES_KEY);
  if DependenciesElement <> nil then begin
    FDependencies := TDependencies.Create(Self, DependenciesElement);
    FDependencies.Parse;
  end else FDependencies := nil;
end;

procedure TProject.ParseProperties;
begin
  var PropertiesElement := GetElement(PROPERTIES_KEY);
  FProperties := TProperties.Create(Self, PropertiesElement);
  FProperties.Parse(PropertiesElement <> nil);
end;

procedure TProject.ParseProxy;
begin
  var ProxyElement := GetElement(PROXY_KEY);
  if ProxyElement <> nil then begin
    FProxy := TProxy.Create(Self, ProxyElement);
    FProxy.Parse;
  end else FProxy := nil;
end;

procedure TProject.ParseRepositories;
begin
  var RepositoriesElement := GetElement(REPOSITORIES_KEY);
  FRepositories := TRepositories.Create(Self, RepositoriesElement);
  if RepositoriesElement <> nil then begin
    FRepositories.Parse;
  end;
end;

function TDocumentBase.ChildCount: Integer;
begin
  Result := FDoc.ChildCount(FElement);
end;

constructor TDocumentBase.Create(AProject: TProject; AElement: PsanXMLObjElement);
begin
  FProject := AProject;
  FDoc := AProject.Document;
  FElement := AElement;
  SetLocation(AElement);
end;

procedure TDocumentBase.Debug(const Format: string; const Args: array of const);
begin
  Debug(System.SysUtils.Format(Format, Args));
end;

procedure TDocumentBase.Error(const Format: string; const Args: array of const);
begin
  Error(System.SysUtils.Format(Format, Args));
end;

function TDocumentBase.GetAttributeValue(const AttrName: string): string;
begin
  Result := FDoc.GetAttributeValue(FElement, AttrName).Trim;
end;

function TDocumentBase.GetElement(AElementIndex: Integer): PsanXMLObjElement;
begin
  Result := FDoc.GetElement(FElement, AElementIndex);
end;

function TDocumentBase.GetChildElementValue(AElementName: string): string;
begin
  var E := GetElement(AElementName);
  if E <> nil then begin
    Result := StrPas(E.pValue).Trim;
  end else Result := '';
end;

function TDocumentBase.GetName: string;
begin
  if FElement <> nil then begin
    Result := StrPas(FElement.pName).Trim;
  end else Result := '';
end;

function TDocumentBase.GetValue: string;
begin
  if FElement <> nil then begin
    Result := StrPas(FElement.pValue).Trim;
  end else Result := '';
end;

function TDocumentBase.iif<T>(B: Boolean; T1, T2: T): T;
begin
  if B then Result := T1 else Result := T2;
end;

function TDocumentBase.GetElement(AElementName: string): PsanXMLObjElement;
begin
  Result := FDoc.GetElement(FElement, AElementName);
end;

procedure TDocumentBase.Info(const Format: string; const Args: array of const);
begin
  Info(System.SysUtils.Format(Format, Args));
end;

{ TProxy }

destructor TProxy.Destroy;
begin
  if FIncludeFilters <> nil then FreeAndNil(FIncludeFilters);
  if FExcludeFilters <> nil then FreeAndNil(FExcludeFilters);
  inherited;
end;

procedure TProxy.Parse;
begin
  Debug('Begin process proxy config');
  FHost := GetAttributeValue('host');
  try
    FPort := StrToInt(GetAttributeValue('port'));
  except
    on E: Exception do begin
      Error('Wrong port number: %s', [E.Message]);
    end;
  end;
  FUser := GetAttributeValue('user');
  FPassword := GetAttributeValue('password');

  var strEnabled := GetAttributeValue('enabled');
  FEnabled := strEnabled.IsEmpty or (CompareText(strEnabled, 'true') = 0);

  var IncludeItems := GetElement('includes');
  if IncludeItems <> nil then begin
    FIncludeFilters := TProxyFilters.Create(Project, IncludeItems);
    FIncludeFilters.Debug('Handle include proxy filter');
    FIncludeFilters.Parse;
  end else FIncludeFilters := nil;

  var ExcludeItems := GetElement('excludes');
  if ExcludeItems <> nil then begin
    FExcludeFilters := TProxyFilters.Create(Project, ExcludeItems);
    FExcludeFilters.Debug('Handle exclude proxy filter');
    FExcludeFilters.Parse;
  end;

  Debug('End process proxy config: %s', [FHost]);
end;

{ TDependencies }

destructor TDependencies.Destroy;
begin
  if FDependencies <> nil then begin
    for var I := FDependencies.Count-1 downto 0 do begin
      TDependencie(FDependencies[I]).Free;
      FDependencies.Delete(I);
    end;
    FreeAndNil(FDependencies);
  end;
  inherited;
end;

procedure TDependencies.DownloadPackage;
begin
  Info('Begin download source code');
  for var Dependencie in FDependencies do begin
    Dependencie.DownloadPackage;
  end;
  Info('End download source code');
end;

procedure TDependencies.GetSearchPath(Strings: TStrings);
var
  FullPath: string;
begin
  Info('Begin config search path');
  if FDependencies = nil then Exit;
  for var Dependencie in FDependencies do begin
    Dependencie.GetSearchPath(Strings);
  end;

  {$region 'Validate all the path exists'}
  Info('Validate all the search path exists');
  var LibRoot := IncludeTrailingPathDelimiter(Project.Properties.GetValue(LIBS_ROOT_KEY));
  if LibRoot = '' then
    LibRoot := LIBS_ROOT_DEFAULT;
  var ProjectDir := ExtractFileDir(Project.ProjectName);

  for var Path in Strings do begin
    if Path.StartsWith(LIBS_ROOT_DEFAULT, True) then begin
      FullPath := TPath.Combine(ProjectDir, Path.Substring(2));
    end else begin
      FullPath := Path.Replace(Format('$(%s)%s', [LIBS_ROOT_KEY, TPath.DirectorySeparatorChar]), LibRoot, [rfIgnoreCase]);
    end;

    if not TDirectory.Exists(FullPath) then begin
      Warn('Path not exists "%s"', [FullPath]);
      Warn('You need check it your self', [FullPath]);
    end;
  end;
  {$endregion}
  Info('End config search path');
end;

procedure TDependencies.Parse;
begin
  Debug('Begin process dependencies');
  var Count := ChildCount;
  if Count > 0 then FDependencies := TList<TDependencie>.Create else FDependencies := nil;
  for var I := 0 to Count-1 do begin
    var DependencieElement := GetElement(I);
    var Dependencie := TDependencie.Create(Project, DependencieElement);
    Dependencie.Parse;
    FDependencies.Add(Dependencie);
  end;
  Debug('End process dependencies');
end;

{ TDependencie }

destructor TDependencie.Destroy;
begin
  if FLibPaths <> nil then FreeAndNil(FLibPaths);
  inherited;
end;

procedure TDependencie.DownloadPackage;

  function HttpDownload(const URL, DownloadDir: string): string;
  var
    Http: THttpClient;
    Stream: TFileStream;
  begin
    Info('Begin download "%s"', [URL]);

    Result := TPath.Combine(DownloadDir, FVersion) + '.' + FType;

    if not TFile.Exists(Result) then begin
      if not TDirectory.Exists(DownloadDir) then begin
        ForceDirectories(DownloadDir);
      end;

      Http := THttpClient.Create;
      try
        Http.ConnectionTimeout := 10;
        Http.UserAgent := 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36 Edg/91.0.864.59';
        if (Project.Proxy <> nil) and Project.Proxy.Enabled then begin
          http.ProxySettings.Create(Project.Proxy.Host, Project.Proxy.port, Project.Proxy.UnitName, Project.Proxy.Password);
        end;

        var tmpFile := ChangeFileExt(Result, '.tmp');
        try
          if TFile.Exists(tmpFile) then
            TFile.Delete(tmpFile);

          Stream := TFileStream.Create(tmpFile, fmCreate);
          try
            var Response := Http.Get(URL, Stream);
            for var h in Response.Headers do begin
              Debug('Response head: %s=%s', [h.Name, h.Value]);
            end;
          finally
            FreeAndNil(Stream);
          end;
          TFile.Move(tmpFile, Result);
        except
          on NetExcept: ENetHTTPClientException do begin
            TFile.Delete(tmpFile);
            raise Exception.Create(NetExcept.Message);
          end;
        end;
      finally
        FreeAndNil(Http);
      end;
    end else Info('Download file exists "%s", skipped', [Result]);
    Info('End download "%s"', [URL]);
  end;

  function ResolveURL(const Repo: string): string;
  begin
    var LRepo := Repo.Replace('\', '/',[rfReplaceAll]);
    var URL := LRepo.Replace('$(group)', FGroup.Replace('.', '/', [rfReplaceAll]), [rfReplaceAll, rfIgnoreCase]);
    URL := URL.Replace('$(artifactId)', ArtifactId, [rfReplaceAll, rfIgnoreCase]);
    URL := URL.Replace('$(version)', FVersion, [rfReplaceAll, rfIgnoreCase]);
    URL := URL.Replace('$(compileVersion)', FVersion, [rfReplaceAll, rfIgnoreCase]);
    URL := URL.Replace('$(type)', FType, [rfReplaceAll, rfIgnoreCase]);
    Result := URL;
  end;

  function Download(DownloadDir: string; var ZipFile: string): Boolean;
  var
    LRepo: string;
  begin
    if FRepo.IsEmpty then begin
      Result := False;
      for var R in Project.Repositories do begin
        LRepo := iif(R.URL.EndsWith('/'), R.URL, R.URL + '/') + FPath + '/' + FName;

        try
          var URL := ResolveURL(LRepo);
          ZipFile := HttpDownload(URL, DownloadDir);
          Result := True;
        except
          on E: Exception do begin
            Error('Download file error: %s', [E.Message]);
            Result := False;
          end;
        end;

        if Result then Break;
      end;
    end else begin
      try
        var URL := ResolveURL(FRepo);
        ZipFile := HttpDownload(URL, DownloadDir);
        Result := True;
      except
        on E: Exception do begin
          Error('Download file error: %s', [E.Message]);
          Result := False;
        end;
      end;
    end;
  end;

var
  ZipFile, DownloadDir: string;
begin
  if FLibPaths = nil then Exit;

  var LibsRoot := Project.Properties.GetValue(LIBS_ROOT_KEY);
  if LibsRoot = '' then
    LibsRoot := LIBS_ROOT_DEFAULT;
  LibsRoot := IncludeTrailingPathDelimiter(LibsRoot);
  var ProjectDir := ExtractFileDir(Project.ProjectName);

  if LibsRoot.Equals(LIBS_ROOT_DEFAULT) then begin
    var LibPath := TPath.Combine(LIBS_ROOT_DEFAULT.Substring(2), FPath);
    DownloadDir := TPath.Combine(ProjectDir, LibPath);
  end else begin
    DownloadDir := TPath.Combine(LibsRoot, FPath);
  end;

  try
    if Download(DownloadDir, ZipFile) then begin
      Info('Begin unzip "%s"', [ZipFile]);
      Unzip(ZipFile, DownloadDir);
      Info('End unzip "%s"', [ZipFile]);
    end;
  except
    on E: Exception do
      Error('Unzip file error: %s', [E.Message]);
  end;
end;

procedure TDependencie.GetSearchPath(Strings: TStrings);
begin
  if FLibPaths <> nil then
    FLibPaths.GetSearchPath(Strings);
end;

procedure TDependencie.Parse;
begin
  FGroup := GetChildElementValue('group');
  FArtifactId := GetChildElementValue('artifactId');
  FVersion := GetChildElementValue('version');
  FCompileVersion := GetChildElementValue('compileVersion');

  var LType := GetChildElementValue('type');
  FType := iif<string>(LType.IsEmpty, 'zip', LType);

  //[FGroup]/[artifactId]/[version](-[folderItegRev])/[artifactId]-[version](-[fileItegRev])(-[compileVersion]).[type]
  if FCompileVersion.IsEmpty then begin
    FPath := Format('%s\%s\%s', [FGroup, FArtifactId, FVersion]);
    FName := Format('%s-%s.%s', [FArtifactId, FVersion, FType]);
  end else begin
    // 最后一个是Delphi的版本号，用于dcu
    FPath := Format('%s\%s\%s\%s', [FGroup, FArtifactId, FVersion, FCompileVersion]);
    FName := Format('%s-%s-%s.%s', [FArtifactId, FVersion, FCompileVersion, FType]);
  end;

  var Lib := GetElement('searchPath');
  if Lib <> nil then begin
    FLibPaths := TLibPaths.Create(Project, Lib);
    FLibPaths.Parse(FPath);
  end else FLibPaths := nil;

  FRepo := GetChildElementValue('repo');

  Debug('Found dependencie config: %s\%s\%s', [FGroup, FArtifactId, FVersion]);
end;

procedure TDependencie.Unzip(const ZipFile, FilesRoot: string);
var
  Zip: TZipFile;
  I: Integer;
  TopFolder: string;
  HasTopFolder: Boolean;

  ZipStream: TStream;
  ZipHeader: TZipHeader;

  ZippedFileName: string;
  FileName: string;
begin
  if not TZipFile.IsValid(ZipFile) then begin
    Error('Invalidate zip file');
    Exit;
  end;

  Zip := TZipFile.Create;
  try
    Zip.Open(ZipFile, zmRead);

    TopFolder := '';
    HasTopFolder := True;
    I := 0;
    for FileName in Zip.FileNames do begin
      var Folders := FileName.Split(['/']);
      if I = 0 then begin
        TopFolder := Folders[0];
        Inc(I);
      end;

      if TopFolder <> Folders[0] then begin
        HasTopFolder := False;
        Break;
      end;
    end;

    for ZippedFileName in Zip.FileNames do begin
      if ZippedFileName.EndsWith('/') then Continue;

      Debug('Zipped file name: %s', [ZippedFileName]);
      if HasTopFolder then begin
        FileName := ZippedFileName.Substring(TopFolder.Length +1);
      end else begin
        FileName := ZippedFileName;
      end;

      var FullName := TPath.Combine(FilesRoot, FileName.Replace('/','\'));

      if TFile.Exists(FullName) then begin
        TFile.Delete(FullName);
      end else begin
        var FileDir := ExtractFileDir(FullName);
        if not TDirectory.Exists(FileDir) then
          TDirectory.CreateDirectory(FileDir);
      end;

      try
        Zip.Read(ZippedFileName, ZipStream, ZipHeader);
        var F := TFileStream.Create(FullName, fmCreate);
        try
          F.CopyFrom(ZipStream, ZipHeader.UncompressedSize);
        finally
          FreeAndNil(F);
        end;
      except
        on E: Exception do begin
          Debug('Un compress %s error', [ZippedFileName]);
        end;
      end;
    end;
  finally
    FreeAndNil(Zip);
  end;
end;

{ TProperties }

procedure TProperties.AddCustomProperties;
begin
  var Count := ChildCount;
  for var I := 0 to Count-1 do begin
    var PropertyElement := GetElement(I);
    var Prop := TProperty.Create(Project, PropertyElement);
    Prop.Parse;
    // 重复的属性先入为主
    var ObjIndex := FProperties.IndexOf(Prop.Name);
    if ObjIndex <> -1 then begin
      Warn(DUP_MESSAGE, ['custom', Prop.Name, Prop.Value]);
      FreeAndNil(Prop);
    end else FProperties.AddObject(Prop.Name, Prop);
  end;
end;

procedure TProperties.AddSpecialProperties;
begin
//  var LibsRoot := Config.LibsRoot;
//  if LibsRoot <> '' then begin
//    var Prop := TProperty.Create(Project, nil);
//    Prop.AddSpecialProperty(LIBS_ROOT_KEY, LibsRoot);
//    FProperties.AddObject(Prop.Name, Prop);
//  end;
end;

procedure TProperties.AddSystemProperties;
begin
  GetEnvList(function(const AName, AValue: string): Boolean begin
    // 重复的属性先入为主，自定义的优先
    var ObjIndex := FProperties.IndexOf(AName);
    if ObjIndex = -1 then begin
      var Prop := TProperty.Create(Project, nil);
      Prop.AddSystemProperty(AName, AValue);
      FProperties.AddObject(Prop.Name, Prop);
    end else Warn(DUP_MESSAGE, ['system', AName, AValue]);

    Result := True;
  end);
end;

destructor TProperties.Destroy;
begin
  for var I := FProperties.Count-1 downto 0 do begin
    TProperty(FProperties.Objects[I]).Free;
    FProperties.Delete(I);
  end;
  FreeAndNil(FProperties);
  inherited;
end;

function TProperties.GetValue(const AName: string): string;
begin
  var i := FProperties.IndexOf(AName);
  if i <> -1 then begin
    Result := TProperty(FProperties.Objects[I]).Value;
  end else begin
    Result := '';
  end;
end;

procedure TProperties.Parse(ExistsCustomeProperties: Boolean);
begin
  Debug('Begin process properties');
  FProperties := TStringList.Create;
  AddSpecialProperties;

  AddSystemProperties;

  if ExistsCustomeProperties then begin
    AddCustomProperties;
  end;

  ResolvePropertyValues;
  Debug('End process properties');
end;

procedure TProperties.ResolvePropertyValues;
begin

end;

{ TProperty }

procedure TProperty.AddSpecialProperty(AName, AValue: string);
begin
  FName := AName.Trim;
  FValue := AValue.Trim;
  Debug('Add special property: %s', [ToString]);
end;

procedure TProperty.AddSystemProperty(AName, AValue: string);
begin
  FName := AName.Trim;
  FValue := AValue.Trim;
  Debug('Add system property: %s', [ToString]);
end;

procedure TProperty.Parse;
begin
  FName := GetName;
  FValue := GetValue;
  Debug('Add custom property: %s', [ToString]);
end;

function TProperty.ToString: string;
begin
  Result := Format('Key=%s, Value=%s',[FName, FValue]);
end;

{ TProxyFilters }

destructor TProxyFilters.Destroy;
begin
  if FFilters <> nil then begin
    for var I := FFilters.Count-1 downto 0 do begin
      TProxyFilter(FFilters[I]).Free;
      FFilters.Delete(I);
    end;
    FreeAndNil(FFilters);
  end;

  inherited;
end;

procedure TProxyFilters.Parse;
begin
  var Count := ChildCount;
  if Count > 0 then begin
    FFilters := TList<TProxyFilter>.Create;
    for var I := 0 to Count-1 do begin
      var Filter := GetElement(I);
      var ProxyFilter := TProxyFilter.Create(Project, Filter);
      ProxyFilter.Parse;
      FFilters.Add(ProxyFilter);
    end;
  end else FFilters := nil;;
end;

{ TProxyFilter }

destructor TProxyFilter.Destroy;
begin
  inherited;
end;

procedure TProxyFilter.Parse;
begin
  FFilter := GetValue;
  Debug('Found filter: %s', [FFilter]);
end;

{ TLibPaths }

destructor TLibPaths.Destroy;
begin
  if FPaths <> nil then begin
    for var I := FPaths.Count-1 downto 0 do begin
      TLibPath(FPaths.Objects[I]).Free;
      FPaths.Delete(I);
    end;
    FreeAndNil(FPaths);
  end;

  inherited;
end;

function TLibPaths.GetSearchPath(Strings: TStrings): string;
var
  FullPath: string;
begin
  if FPaths = nil then Exit;

  var LibRoot := Project.Properties.GetValue(LIBS_ROOT_KEY);
  if LibRoot = '' then
    LibRoot := LIBS_ROOT_DEFAULT;
  LibRoot := IncludeTrailingPathDelimiter(LibRoot);
  Debug('%s is %s', [LIBS_ROOT_KEY, LibRoot]);

  var ProjectDir := ExtractFileDir(Project.ProjectName);

  for var I := 0 to FPaths.Count-1 do begin
    var Path := TPath.Combine(LibRoot, FPaths[I]);

    {$region 'Validate the path exists'}
    if CompareText(LibRoot,LIBS_ROOT_DEFAULT) = 0 then begin
      FullPath := TPath.Combine(ProjectDir, Path);
    end else begin
      FullPath := Path;
    end;

    if not TDirectory.Exists(FullPath) then begin
      var LibPath := TLibPath(FPaths.Objects[I]);
      LibPath.Warn('Configurable path not exists: %s', [Path]);
    end;
    {$endregion}

    // 用环境变量替换
    if CompareText(LibRoot, LIBS_ROOT_DEFAULT) <> 0 then
      Path := Path.Replace(LibRoot, Format('$(%s)%s',[LIBS_ROOT_KEY, TPath.DirectorySeparatorChar]), [rfIgnoreCase]);

    Strings.Add(Path);
  end;
end;

procedure TLibPaths.Parse(const PathPrefix: string);
begin
  Debug('Begin process search path');
  var Count := ChildCount;
  if Count > 0 then begin
    FPaths := TStringList.Create('"', ';', [soStrictDelimiter]);

    for var I := 0 to Count-1 do begin
      var PathElement := GetElement(I);
      var LibPath := TLibPath.Create(Project, PathElement);
      LibPath.Parse(PathPrefix);
      var ItemIndex := FPaths.IndexOf(LibPath.Path);
      if ItemIndex = -1 then begin
        FPaths.AddObject(LibPath.Path, LibPath);
      end else begin
        Warn('Found duplicates path %s, skip it', [LibPath.Path]);
        FreeAndNil(LibPath);
      end;
    end;
  end else FPaths := nil;
  Debug('End process search path');
end;

{ TLibPath }

procedure TLibPath.Parse(PathPrefix: string);
begin
  FPath := GetValue;
  while (FPath.IndexOf('\') = 0) or (FPath.IndexOf('/') = 0) do begin
    FPath := FPath.Substring(1);
  end;
  if FPath = '.' then FPath := '';
  if Length(FPath) = 0 then begin
    FPath := PathPrefix;
  end else begin
    FPath := TPath.Combine(PathPrefix, FPath);
  end;
end;

{ TProjects }

procedure TProjects.ApplySearchPath(const AProjectName: string);
var
  Mapping: TProjectMapping;
begin
  if not FProjects.TryGetValue(AProjectName, Mapping) then Exit;

  var PathList := TStringList.Create('"', ';', [soStrictDelimiter]);
  try
    PathList.Duplicates := dupIgnore;
    PathList.Sorted := True;
    PathList.CaseSensitive := False;

    var Project := Mapping.Project;
    var DelphiProject := Mapping.DelphiProject;
    PathList.DelimitedText := DelphiProject.ProjectOptions.Values['DCC_UnitSearchPath'];

    Project.Dependencies.GetSearchPath(PathList);
    if PathList.Count > 0 then begin
      DelphiProject.ProjectOptions.Values['DCC_UnitSearchPath'] := PathList.DelimitedText;
    end;
  finally
    FreeAndNil(PathList);
  end;
end;

procedure TProjects.ApplyProjectSearchPath(const AProjectName: string);
begin
  if AProjectName = '' then begin
    for var S in FProjects.Keys do begin
      ApplySearchPath(S);
    end;
  end else begin
    ApplySearchPath(AProjectName);
  end;
end;

constructor TProjects.Create(ASearchPathManager: ISearchPathManager);
begin
  FSearchPath := ASearchPathManager;
  FProjects := TDictionary<string, TProjectMapping>.Create;
  Init;
end;

destructor TProjects.Destroy;
var
  Mapping: TProjectMapping;
begin
  for var K in FProjects.Keys do begin
    if FProjects.TryGetValue(K, Mapping) then
      FreeAndNil(Mapping);
  end;
  FreeAndNil(FProjects);

  if FRootProject <> nil then
    FreeAndNil(FRootProject);

  inherited;
end;

procedure TProjects.Init;
begin
  var ModServices := BorlandIDEServices as IOTAModuleServices;
  FRootProject := nil;
  var ProjectGroup := ModServices.MainProjectGroup;
  var LProjectName := ChangeFileExt(ProjectGroup.FileName, '.pom.xml');
  if TFile.Exists(LProjectName) then begin
    FRootProject := TProject.Create(nil, LProjectName);
    FRootProject.Parse;
  end;

  for var I := 0 to ProjectGroup.ProjectCount-1 do begin
    var DelphiProject := ProjectGroup.Projects[I];
    LProjectName := ChangeFileExt(DelphiProject.FileName, '.pom.xml');
    var LProject := TProject.Create(FRootProject, LProjectName);
    LProject.LogMessage := ProjectLogMessage;
    var Mapping := TProjectMapping.Create(LProject, DelphiProject);
    FProjects.Add(LProjectName, Mapping);
  end;
end;

procedure TProjects.ProjectLogMessage(Sender: TObject; Text: string; LogLevel: TLogLevel);
var
  MessageContext: TMessageContext;
begin
  if (Ord(LogLevel) - Ord(TDocumentBase(Sender).Project.LogLevel)) < 0 then Exit;

  MessageContext.FileName := TDocumentBase(Sender).Project.ProjectName;
  MessageContext.Row := TDocumentBase(Sender).Row;
  MessageContext.Col := TDocumentBase(Sender).Col;
  MessageContext.LogLevel := LogLevel;

  case LogLevel of
    llDebug: begin
      MessageContext.ForeColour := clWebWhite;
    end;
    llInfo: begin
      MessageContext.ForeColour := clWebGreen;
    end;
    llWarn: begin
      MessageContext.ForeColour := clWebGold;
    end;
    else begin
      MessageContext.ForeColour := clWebDarkRed;
    end;
  end;

  FSearchPath.OutputMessage(Text, MessageContext);
end;

procedure TProjects.UpdateProjectLib(const AProjectName: string);
var
  Mapping: TProjectMapping;
begin
  if FProjects.TryGetValue(AProjectName, Mapping) then
    Mapping.Project.Dependencies.DownloadPackage;
end;

procedure TProjects.UpdateProjectLibs(const AProjectName: string);
begin
  if AProjectName = '' then begin
    for var S in FProjects.Keys do begin
      UpdateProjectLib(S);
    end;
  end else begin
    UpdateProjectLib(AProjectName);
  end;
end;

procedure TProjects.ParseProject(const AProjectName: string);
var
  Mapping: TProjectMapping;
begin
  if FProjects.TryGetValue(AProjectName, Mapping) then begin
    var ModuleInfo := Mapping.DelphiProject.FindModuleInfo(AProjectName);
    if ModuleInfo <> nil then begin
      ModuleInfo.OpenModule.Save(False, True);
    end;
    Mapping.Project.Parse;
  end;
end;

procedure TProjects.Parse(const AProjectName: string);
begin
  if AProjectName = '' then begin
    for var S in FProjects.Keys do begin
      ParseProject(S);
    end;
  end else begin
    ParseProject(AProjectName);
  end;
end;

{ TProjectMapping }

constructor TProjectMapping.Create(AProject: TProject; ADelphiProject: IOTAProject);
begin
  FProject := AProject;
  FDelphiProject := ADelphiProject;
end;

destructor TProjectMapping.Destroy;
begin
  FreeAndNil(FProject);
  FDelphiProject := nil;

  inherited;
end;

{ TRepositories }

constructor TRepositories.Create(AProject: TProject; AElement: PsanXMLObjElement);
begin
  inherited Create(AProject, AElement);
  FRepositories := TList<TRepository>.Create;
end;

destructor TRepositories.Destroy;
begin
  if FRepositories <> nil then begin
    for var I := 0 to FRepositories.Count-1 do begin
      TRepository(FRepositories[I]).Free;
    end;
    FRepositories.Clear;
  end;
  inherited;
end;

procedure TRepositories.Parse;
begin
  Debug('Begin process repositories');
  var Count := ChildCount;
  for var I := 0 to Count-1 do begin
    var RepositoryElement := GetElement(I);
    var Repository := TRepository.Create(Project, RepositoryElement);
    Repository.Parse;
    FRepositories.Add(Repository);
  end;
  Debug('End process repositories');
end;

{ TRepository }

procedure TRepository.Parse;
begin
  FId := GetAttributeValue('id');
  FName := GetAttributeValue('name');
  FURL := GetValue;
  Debug(Format('%s %s %s', [Fid, FName, FURL]));
end;

end.
