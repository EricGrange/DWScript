# DWScript WebServer Configuration Guide

This document describes the configuration options available in the `options.json` file for the DWScript WebServer.

The default options can be obtained by calling with the `/options-defaults` parameter. The JSON is comprised of all the sections below.

## Service Configuration

The Service section configures Windows service-related settings.

```json
{
    "Service": {
        "Name": "DWSServer",          // Windows service name
        "DisplayName": "DWScript WebServer",  // Windows service display name
        "Description": "DWScript WebServer Service",  // Windows service description
        "DWSErrorLogDirectory": ""    // Directory for error logs
    }
}
```

## Server Configuration

The Server section controls the web server behavior and capabilities.

```json
{
    "Server": {
        "Name": "DWScript",          // Server name reported in responses
        "Port": 888,                 // HTTP port (0 = disabled)
        "RelativeURI": "",           // Base URI path for HTTP
        "DomainName": "+",           // HTTP domain name
        "SSLPort": 0,               // HTTPS port (0 = disabled)
        "SSLDomainName": "+",       // HTTPS domain name
        "SSLRelativeURI": "",       // Base URI path for HTTPS
        "Domains": [],              // Additional domain configurations
        "Compression": true,        // Enable HTTP compression
        "WWWPath": "",             // Web root directory (defaults to ./www)
        "Authentication": [],       // Authentication methods ("Basic", "Digest", "NTLM", "Negotiate", "*")
        "WorkerThreads": 16,       // Number of worker threads
        "DWSErrorLogDirectory": "", // DWScript error log directory
        "DWSExceptionLogDirectory": "", // Unhandled exception log directory
        "LogDirectory": "",        // NCSA format log directory
        "MaxConnections": 0,       // Maximum concurrent connections (0 = unlimited)
        "MaxQueueLength": 0,       // HTTP server queue length (0 = default)
        "MaxBandwidth": 0,         // Maximum bandwidth in bytes/sec (0 = unlimited)
        "MaxInputLength": 0,       // Maximum request size in bytes (0 = 10MB)
        "AutoRedirectFolders": true, // Auto-redirect missing trailing slashes
        "ScriptedExtensions": [".dws"], // Extensions processed as scripts
        "P2JSExtensions": [".p2js", ".pas.js"], // Pascal to JavaScript extensions
        "MethodsNotAllowed": ["TRACE"] // Blocked HTTP methods
    }
}
```

## CPU Configuration

Controls CPU usage and affinity settings.

```json
{
    "CPU": {
        "UsageLimit": 0,    // CPU usage limit percentage (0-100, 0 = unlimited)
        "Affinity": 0       // CPU core affinity bitmask (0 = no affinity)
    }
}
```

## DWScript Configuration

Controls script execution and compilation settings.

```json
{
    "DWScript": {
        "TimeoutMSec": 3000,         // Script execution timeout (0 = unlimited, not recommended)
        "WorkerTimeoutMSec": 30000,  // Worker thread timeout
        "StackChunkSize": 300,       // Stack growth chunk size
        "StackMaxSize": 10000,       // Maximum stack size per script
        "MaxRecursionDepth": 512,    // Maximum recursion depth
        "LibraryPaths": ["%www%\\.lib"], // Library paths (%www% = web root)
        "WorkPaths": ["%www%"],      // Allowed file operation paths
        "DBPaths": null,             // Database file paths (null = same as WorkPaths)
        "Conditionals": [],          // Predefined conditional defines
        "PatternOpen": "<?pas",      // HTML filter opening pattern
        "PatternEval": "=",          // HTML filter evaluation pattern
        "PatternClose": "?>",        // HTML filter closing pattern
        "MaxWorkersPerQueue": 32,    // Maximum workers per queue
        "Startup": "%www%\\.startup.pas",  // Startup script path
        "Shutdown": "%www%\\.shutdown.pas", // Shutdown script path
        "JIT": true,                 // Enable JIT compilation
        "COM": false,                // Enable COM support (breaks sandboxing)
        "OP4JS": {                   // Pascal to JavaScript optimization options
            "NoRangeChecks": true,
            "NoCheckInstantiated": true,
            "NoCheckLoopStep": true,
            "NoConditions": true,
            "NoSourceLocations": true,
            "Obfuscate": false,
            "OptimizeForSize": false,
            "SmartLink": true,
            "DeVirtualize": true,
            "NoRTTI": true
        }
    }
}
```

### Important Notes:

1. Path variables:
   - `%www%` refers to the web root directory defined with the `WWWPath` setting in server configuration
   - Relative paths are resolved from the executable's location

2. Security considerations:
   - Setting `COM` to true breaks sandboxing
   - Always set appropriate timeout values
   - Carefully consider authentication methods
   - Restrict file operation paths appropriately

3. Performance tuning:
   - Adjust `WorkerThreads` based on server capacity
   - Monitor `MaxConnections` and `MaxBandwidth` in production
   - Use `CPU.UsageLimit` to prevent resource exhaustion
