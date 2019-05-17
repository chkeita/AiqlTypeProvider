namespace AiqlTest

open Xunit
open Xunit.Abstractions
open System.Net.Http
open System
open ExpressionBuilder.ResultParer
open Newtonsoft.Json
open FSharp.Control

type Request = {
    timestamp: DateTime
    id: string
    source: string
    name: string
    url: string
    success: string
    resultCode: string
    duration: float
    performanceBucket: string
    customDimensions: string
    customMeasurements: string
    operation_Name: string
    operation_Id: string
    operation_ParentId: string
    operation_SyntheticSource: string
    session_Id: string
    user_Id: string
    user_AuthenticatedId: string
    user_AccountId: string
    application_Version: string
    client_Type: string
    client_Model: string
    client_OS: string
    client_IP: string
    client_City: string
    client_StateOrProvince: string
    client_CountryOrRegion: string
    client_Browser: string
    cloud_RoleName: string
    cloud_RoleInstance: string
    appId: string
    appName: string
    iKey: string
    sdkVersion: string
    itemId: string
    itemType: string
    itemCount: int64
} 

module ResultParserTest = 
    open Newtonsoft.Json.Linq

    [<Fact>]
    let canParseResult () = 
        
        let filePath = "sampleAiqlResult.json"
        use fileStream = System.IO.File.OpenRead filePath
        let resultSequence = readResults<Request>(fileStream)
        Assert.All(resultSequence |> AsyncSeq.toBlockingSeq, fun elem -> Assert.NotEqual( Unchecked.defaultof<DateTime>, elem.timestamp ))


