# ABAPilot ABAP Connector

A lightweight, table-driven REST API framework for SAP ECC 6.0 (and higher).

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
- **Works on ECC 6.0** — tested on EHP 0 through 8
- **Runtime control** — enable/disable endpoints without transport
- **Authorization support** — per-endpoint auth object configuration
- **SM30 maintenance** — standard SAP tooling, standard transports

---

## Installation

### Prerequisites

- SAP ECC 6.0 or higher (also works on S/4HANA)
- Developer access to create objects in a custom namespace
- [abapGit](https://abapgit.org/) installed

### Steps

1. **Clone via abapGit**
   ```
   Repository URL: https://github.com/[your-username]/abapilot-connector
   Package: ZABAPILOT (or your choice)
   ```

2. **Activate all objects**
   - Table: `ZAGENT_ENDPOINTS`
   - Table maintenance: `ZAGENT_ENDPOINTS` (SM30)
   - Class: `ZCL_AGENT_HTTP_HANDLER`
   - Sample handlers: `ZCL_AGENT_*`

3. **Create SICF node**
   - Transaction: `SICF`
   - Path: `/sap/bc/zagent` (or your choice)
   - Handler: `ZCL_AGENT_HTTP_HANDLER`

4. **Configure endpoints**
   - Transaction: `SM30`
   - Table: `ZAGENT_ENDPOINTS`
   - Add your endpoint mappings

5. **Test**
   ```
   GET https://<host>:<port>/sap/bc/zagent/your/endpoint
   ```

---

## Quick Start

### 1. Create a handler class

```abap
CLASS zcl_my_handler DEFINITION PUBLIC.
  PUBLIC SECTION.
    METHODS hello
      IMPORTING
        io_request  TYPE REF TO if_http_request
        io_response TYPE REF TO if_http_response.
ENDCLASS.

CLASS zcl_my_handler IMPLEMENTATION.
  METHOD hello.
    DATA(lv_name) = io_request->get_form_field( 'name' ).
    
    io_response->set_content_type( 'application/json' ).
    io_response->set_cdata( |{{ "message": "Hello { lv_name }!" }}| ).
  ENDMETHOD.
ENDCLASS.
```

### 2. Add endpoint configuration (SM30)

| ENDPOINT | HANDLER_CLASS | METHOD_NAME | HTTP_METHOD | ACTIVE |
|----------|---------------|-------------|-------------|--------|
| /hello   | ZCL_MY_HANDLER | HELLO | GET | X |

### 3. Call it

```bash
curl "https://<host>:<port>/sap/bc/zagent/hello?name=World"

# Response:
{ "message": "Hello World!" }
```

No transport. No deployment. Just works.

---

## Configuration Table

### ZAGENT_ENDPOINTS

| Field | Type | Description |
|-------|------|-------------|
| `ENDPOINT` | CHAR(60) | URL path after SICF node (e.g., `/hello`) |
| `HANDLER_CLASS` | SEOCLSNAME | ABAP class to instantiate |
| `METHOD_NAME` | SEOCPDNAME | Method to call |
| `HTTP_METHOD` | CHAR(10) | GET, POST, PUT, DELETE, or * for any |
| `AUTH_OBJECT` | XUOBJECT | Authorization object (optional) |
| `AUTH_FIELD` | XUFIELD | Authorization field (optional) |
| `AUTH_VALUE` | XUVAL | Required auth value (optional) |
| `ACTIVE` | XFELD | X = enabled, blank = disabled |
| `DESCRIPTION` | TEXT80 | Documentation |

---

## Handler Method Signature

All handler methods must follow this signature:

```abap
METHODS <method_name>
  IMPORTING
    io_request  TYPE REF TO if_http_request
    io_response TYPE REF TO if_http_response.
```

### Reading input

```abap
" Query parameters
DATA(lv_param) = io_request->get_form_field( 'param_name' ).

" Request body
DATA(lv_body) = io_request->get_cdata( ).

" Headers
DATA(lv_header) = io_request->get_header_field( 'X-Custom-Header' ).
```

### Writing output

```abap
" Set response
io_response->set_content_type( 'application/json' ).
io_response->set_cdata( lv_json_string ).

" Set status code
io_response->set_status( code = 200 reason = 'OK' ).
```

---

## Authorization

Configure per-endpoint authorization in the config table:

| ENDPOINT | AUTH_OBJECT | AUTH_FIELD | AUTH_VALUE |
|----------|-------------|------------|------------|
| /admin/delete | Z_AGENT | ACTVT | 06 |
| /data/read | Z_AGENT | ACTVT | 03 |

The handler performs `AUTHORITY-CHECK` before calling your method. Failed checks return HTTP 403.

---

## Included Sample Handlers

| Class | Purpose |
|-------|---------|
| `ZCL_AGENT_QUERY` | Execute natural language queries (requires AI backend) |
| `ZCL_AGENT_TABLES` | Read SAP table data with filters |
| `ZCL_AGENT_CODE` | Read/write ABAP source code |
| `ZCL_AGENT_PING` | Simple health check endpoint |

---

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                        HTTP Request                              │
└─────────────────────────────────────────────────────────────────┘
                               │
                               ▼
┌─────────────────────────────────────────────────────────────────┐
│                    SICF Node (/sap/bc/zagent)                   │
│                    Handler: ZCL_AGENT_HTTP_HANDLER              │
└─────────────────────────────────────────────────────────────────┘
                               │
                               ▼
┌─────────────────────────────────────────────────────────────────┐
│  1. Parse endpoint path from request                            │
│  2. Lookup in ZAGENT_ENDPOINTS                                  │
│  3. Check ACTIVE flag                                           │
│  4. Perform AUTHORITY-CHECK (if configured)                     │
│  5. CREATE OBJECT dynamically                                   │
│  6. CALL METHOD dynamically                                     │
│  7. Return response                                             │
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

## Contributing

Issues and PRs welcome. Please:

1. Test on ECC 6.0 (lowest common denominator)
2. Keep dependencies at zero
3. Document any new configuration fields

---

## License

MIT License — use it, modify it, ship it.

---

## Author

**Nicolas** — SAP Technical Consultant
- LinkedIn: [[your-profile](https://www.linkedin.com/in/nicolas-hernandez-abap/)]
- Email: [nicolashernandez1987@gmail.com]

Built in Valencia, Spain 🇪🇸

---

## Acknowledgments

- [abapGit](https://abapgit.org/) — for making ABAP open source possible
- The SAP Community — for feedback and encouragement
