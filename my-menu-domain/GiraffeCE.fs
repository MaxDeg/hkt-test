module Giraffe.ComputationExpression


/// ---------------------------------------------------------------------------
/// --| Configuration Computation Expression
/// ---------------------------------------------------------------------------
module Configuration =
  open System.IO
  open Microsoft.Extensions.Hosting
  open Microsoft.Extensions.Configuration
  open Microsoft.Extensions.Configuration.Ini

  type AppConfiguration =
    { env     : string
      builder : IConfigurationBuilder
    }

  type AppConfigurationBuilder() =
    let create () =
      let config =
        ConfigurationBuilder()
          .AddEnvironmentVariables("ASPNETCORE_")
          .Build()

      { env     = config.[HostDefaults.EnvironmentKey]
                  |> Option.ofObj
                  |> Option.defaultValue Environments.Production
        builder = (ConfigurationBuilder() :> IConfigurationBuilder)
                    .SetBasePath(Directory.GetCurrentDirectory())
      }

    member _.Zero () = create()

    member _.Yield _ = create()

    member _.Run { builder = builder } =
        builder.Build()

    [<CustomOperation("use_command_line")>]
    member _.AddCommandLine(appConfig, args : string[]) =
      appConfig.builder.AddCommandLine(args) |> ignore
      appConfig
      
    [<CustomOperation("use_user_secrets")>]
    member _.AddUserSecrets(appConfig, secretId : string) =
      appConfig.builder.AddUserSecrets(secretId) |> ignore
      appConfig

  /// -------------------------------------------------------------------------
  /// --| Ini Configuration
  /// -------------------------------------------------------------------------
  type AppConfigurationBuilder with
    [<CustomOperation("require_ini_file")>]
    member _.AddRequiredIniFile (appConfig, path : string) =
      appConfig.builder.AddIniFile(path, false) |> ignore
      appConfig
      
    [<CustomOperation("use_ini_file")>]
    member _.AddOptionalIniFile(appConfig, path : string) =
      appConfig.builder.AddIniFile(path, true) |> ignore
      appConfig

    [<CustomOperation("use_env_ini_file")>]
    member _.AddOptionalPerEnvironmentIniFile(appConfig, path) =
      appConfig.builder.AddIniFile(sprintf path appConfig.env, true) |> ignore
      appConfig

  /// -------------------------------------------------------------------------
  /// --| Json Configuration
  /// -------------------------------------------------------------------------  
  type AppConfigurationBuilder with
    [<CustomOperation("require_json_file")>]
    member _.AddRequiredJsonFile (appConfig, path : string) =
      appConfig.builder.AddJsonFile(path, false) |> ignore
      appConfig
      
    [<CustomOperation("use_json_file")>]
    member _.AddOptionalJsonFile(appConfig, path : string) =
      appConfig.builder.AddJsonFile(path, true) |> ignore
      appConfig

    [<CustomOperation("use_env_json_file")>]
    member _.AddOptionalPerEnvironmentJsonFile(appConfig, path) =
      appConfig.builder.AddJsonFile(sprintf path appConfig.env, true) |> ignore
      appConfig

  let configuration = AppConfigurationBuilder()

/// ---------------------------------------------------------------------------
/// --| Application Computation Expression
/// ---------------------------------------------------------------------------

