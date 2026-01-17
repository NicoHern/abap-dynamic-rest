# ABAP Dynamic REST Connector

A lightweight, table-driven REST API framework for SAP ECC 6.0 and S/4HANA.

**Add REST endpoints without code changes. Configure in SM30, call immediately.**

---

## Why?

If you've worked with REST APIs in ECC, you know the pain:

- **CL_REST_ROUTER** requires hardcoded routes — new endpoint = code change + transport
- **SAP Gateway** requires licensing and significant setup
- **RAP** requires S/4HANA

This connector takes a different approach: **endpoints are configuration, not code**.
```
┌─────────────────────────────────────────────────────────────────┐
│  ZAGENT_ENDPOINTS (SM30)                                        │
├──────────────┬─────────────────────┬───────────────┬────────────┤
│ Endpoint     │ Handler Class       │ Method        │ Active     │
├──────────────┼─────────────────────┼───────────────┼────────────┤
│ /query       │ ZCL_AGENT_QUERY     │ EXECUTE       │ X          │
│ /tables/read │ ZCL_AGENT_TABLES    │ READ_TABLE    │ X          │
│ /code/read   │ ZCL_AGENT_CODE      │ READ_CODE     │ X          │
│ /new/endpoint│ ZCL_YOUR_CLASS      │ YOUR_METHOD   │ X   <- add │
└──────────────┴─────────────────────┴───────────────┴────────────┘
```

Add a row. Call it. No deployment.

---

## Features

- **Dynamic routing** — endpoint → class → method mapping via config table
- **Zero dependencies** — pure ABAP, no Gateway, no BTP, no additional licensing
- **Works on ECC 6.0 and S/4HANA** — tested on ECC EHP 0-8 and S/4HANA 2020+
- **Runtime control** — enable/disable endpoints without transport
- **Authorization support** — per-endpoint auth object configuration
- **SM30 maintenance** — standard SAP tooling, standard transports

---

## Installation

### Prerequisites

- SAP ECC 6.0 or S/4HANA
- Developer access to create objects in a custom namespace
- [abapGit](https://abapgit.org/) installed

### Choose Your Branch

| System | Branch | Install Command |
|--------|--------|-----------------|
| **ECC 6.0** | `main` | `https://github.com/NicoHern/abap-dynamic-rest` |
| **S/4HANA** | `s4hana` | `https://github.com/NicoHern/abap-dynamic-rest/tree/s4hana` |

> **Important:** The `main` branch is for ECC 6.0. S/4HANA systems must use the `s4hana` branch due to type compatibility differences in dictionary structures (DD03P, etc.).

### Steps

1. **Clone via abapGit**

   **For ECC 6.0:**
```
   Repository URL: https://github.com/NicoHern/abap-dynamic-rest
   Branch: main
   Package: ZABAPILOT (or your choice)
```

   **For S/4HANA:**
```
   Repository URL: https://github.com/NicoHern/abap-dynamic-rest
   Branch: s4hana
   Package: ZABAPILOT (or your choice)
```

2. **Activate all objects**
   - Table: `ZAGENT_ENDPOINTS`
   - Table Type: `ZAGENT_ENDPOINTS_TT`
   - Classes: `ZCL_AGENT_DISPATCHER`, `ZCL_AGENT_HTTP_HANDLER`, `ZCL_AGENT_HANDLER_BASE`
   - Sample handlers: `ZCL_AGENT_READ`, `ZCL_AGENT_PING`
   - Interface: `ZIF_AGENT_HANDLER`

3. **Create table maintenance (if not imported)**
   - SE11 → Table `ZAGENT_ENDPOINTS`
   - Utilities → Create Table Maintenance Generator
   - Authorization Group: `&NC&`
   - Function Group: `ZAGENT_MAINT`
   - Screen: `0001`

4. **Create SICF node**
   - Transaction: `SICF`
   - Path: `/sap/bc/ZABAPilot`
   - Handler: `ZCL_AGENT_HTTP_HANDLER`
   - Activate the service

5. **Configure endpoints (SM30)**
   - Transaction: `SM30`
   - Table: `ZAGENT_ENDPOINTS`
   - Add endpoint mappings:

   | ENDPOINT_PATH | HTTP_METHOD | HANDLER_CLASS | HANDLER_METHOD | IS_ACTIVE |
   |---------------|-------------|---------------|----------------|-----------|
   | /read_table_structure | POST | ZCL_AGENT_READ | READ_TABLE_STRUCTURE | X |
   | /read_table_data | POST | ZCL_AGENT_READ | READ_TABLE_DATA | X |
   | /read_code | POST | ZCL_AGENT_READ | READ_SOURCE_CODE | X |
   | /syntax_check | POST | ZCL_AGENT_READ | SYNTAX_CHECK | X |

6. **Test**
```
   POST https://<host>:<port>/sap/bc/ZABAPilot/read_table_structure
   Content-Type: application/json

   {"table_name": "T000"}
```

---

## S/4HANA Compatibility Notes

The `s4hana` branch includes fixes for type compatibility issues between ECC and S/4HANA:

| Issue | ECC Code | S/4HANA Fix |
|-------|----------|-------------|
| DD03P field types | `ls_dd03p-fieldname` | `CONV string( ls_dd03p-fieldname )` |
| DDIC structure fields | Direct assignment | `CONV #( )` wrapper |

If you encounter `SYNTAX_ERROR` dumps with messages like:
```
"LS_DD03P-FIELDNAME" is not type-compatible with formal parameter "IV_INPUT"
```

You're likely using the wrong branch. Switch to `s4hana`.

---

## Quick Start

### 1. Create a handler class
```abap
CLASS zcl_my_handler DEFINITION PUBLIC
  INHERITING FROM zcl_agent_handler_base.
  
  PUBLIC SECTION.
    METHODS hello.
ENDCLASS.

CLASS zcl_my_handler IMPLEMENTATION.
  METHOD hello.
    DATA(lv_name) = get_json_value( 'name' ).
    
    set_json_response( |\{"message": "Hello { lv_name }!"\}| ).
  ENDMETHOD.
ENDCLASS.
```

### 2. Add endpoint configuration (SM30)

| ENDPOINT_PATH | HTTP_METHOD | HANDLER_CLASS | HANDLER_METHOD | IS_ACTIVE |
|---------------|-------------|---------------|----------------|-----------|
| /hello | POST | ZCL_MY_HANDLER | HELLO | X |

### 3. Call it
```bash
curl -X POST "https://<host>:<port>/sap/bc/ZABAPilot/hello" \
  -H "Content-Type: application/json" \
  -d '{"name": "World"}'

# Response:
{"message": "Hello World!"}
```

No transport. No deployment. Just works.

---

## Configuration Table

### ZAGENT_ENDPOINTS

| Field | Type | Description |
|-------|------|-------------|
| `ENDPOINT_PATH` | CHAR(100) | URL path after SICF node (e.g., `/hello`) |
| `HTTP_METHOD` | CHAR(10) | GET, POST, PUT, DELETE, or * for any |
| `HANDLER_CLASS` | SEOCLSNAME | ABAP class to instantiate |
| `HANDLER_METHOD` | SEOCPDNAME | Method to call |
| `IS_ACTIVE` | ABAP_BOOL | X = enabled, blank = disabled |

---

## Handler Method Signature

Handler classes should inherit from `ZCL_AGENT_HANDLER_BASE` which provides:
```abap
" Reading input
DATA(lv_value) = get_json_value( 'field_name' ).
DATA(lt_body) = get_request_body( ).

" Writing output
set_json_response( lv_json_string ).
set_error_response( 'Error message' ).
set_status( 400 ).
```

Or implement `ZIF_AGENT_HANDLER` directly for full control:
```abap
METHODS handle_request
  IMPORTING
    io_request  TYPE REF TO if_http_request
    io_response TYPE REF TO if_http_response.
```

---

## Included Handlers

| Class | Purpose |
|-------|---------|
| `ZCL_AGENT_PING` | Health check / echo endpoint |
| `ZCL_AGENT_READ` | Table structure, table data, source code reading |

---

## Architecture
```
┌─────────────────────────────────────────────────────────────────┐
│                        HTTP Request                              │
└─────────────────────────────────────────────────────────────────┘
                               │
                               ▼
┌─────────────────────────────────────────────────────────────────┐
│              SICF Node (/sap/bc/ZABAPilot)                      │
│              Handler: ZCL_AGENT_HTTP_HANDLER                    │
└─────────────────────────────────────────────────────────────────┘
                               │
                               ▼
┌─────────────────────────────────────────────────────────────────┐
│                    ZCL_AGENT_DISPATCHER                         │
│  1. Parse endpoint path from request                            │
│  2. Lookup in ZAGENT_ENDPOINTS                                  │
│  3. Check IS_ACTIVE flag                                        │
│  4. CREATE OBJECT handler dynamically                           │
│  5. CALL METHOD dynamically                                     │
│  6. Return response                                             │
└─────────────────────────────────────────────────────────────────┘
                               │
                               ▼
┌─────────────────────────────────────────────────────────────────┐
│               Your Handler Class + Method                        │
│               (ZCL_MY_HANDLER->MY_METHOD)                       │
└─────────────────────────────────────────────────────────────────┘
```

---

## Comparison

| Feature | This Connector | CL_REST_ROUTER | Gateway | RAP |
|---------|----------------|----------------|---------|-----|
| Add endpoint | SM30 entry | Code change | Model + code | CDS + behavior |
| Transport needed | Config only | Yes | Yes | Yes |
| Runtime enable/disable | ✅ | ❌ | ❌ | ❌ |
| ECC 6.0 support | ✅ | ✅ | ✅ | ❌ |
| S/4HANA support | ✅ | ✅ | ✅ | ✅ |
| Additional licensing | None | None | Required | BTP/S4 |

---

## Use Cases

- **AI integration** — expose SAP data and functions to external AI agents
- **Microservices** — lightweight REST APIs without Gateway overhead
- **Rapid prototyping** — test new endpoints without transport cycles
- **Legacy modernization** — add REST capabilities to ECC systems

---

## Part of ABAPilot

This connector is the ABAP foundation of [ABAPilot](https://crimsonconsultingsl.com/abapilot/), an AI-powered ABAP development tool.

ABAPilot adds:
- Natural language queries against SAP data
- Autonomous ABAP code generation
- Multi-agent pipeline (plan → design → code → review → test → deploy)
- Error learning that improves over time

Interested? [Contact me](mailto:nicolashernandez@crimsonconsultingsl.com) or open an issue.

---

## Troubleshooting

### HTTP 500 / SYNTAX_ERROR dump
- Check ST22 for the exact error
- If it mentions type compatibility (DD03P, etc.), use the `s4hana` branch

### HTTP 404 / Endpoint not found
- Verify SICF node is activated
- Check `ZAGENT_ENDPOINTS` has entries
- Verify endpoint path matches exactly (case-sensitive)

### HTTP 403 / Not authorized
- Check user has required authorizations
- Verify S_TABU_DIS for table access
- Check S_DEVELOP for code operations

---

## Contributing

Issues and PRs welcome. Please:

1. Test on both ECC 6.0 and S/4HANA if possible
2. Keep dependencies at zero
3. Document any new configuration fields
4. Use `CONV string()` for dictionary field assignments (S/4 compatibility)

---

## License

MIT License — use it, modify it, ship it.

---

## Author

**Nicolas Hernandez** — SAP Technical Consultant @ Crimson Consulting
- LinkedIn: [nicolas-hernandez-abap](https://www.linkedin.com/in/nicolas-hernandez-abap/)
- GitHub: [NicoHern](https://github.com/NicoHern)
- Email: nicolashernandez1987@gmail.com

Built in Valencia, Spain 🇪🇸

---

## Acknowledgments

- [abapGit](https://abapgit.org/) — for making ABAP open source possible
- The SAP Community — for feedback and encouragement
