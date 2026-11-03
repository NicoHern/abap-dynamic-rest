# ABAP Dynamic REST

**Canonical source:** [NicoHern/abap-dynamic-rest](https://github.com/NicoHern/abap-dynamic-rest), created and maintained by [Nicolas Hernandez](https://github.com/NicoHern). Forks and mirrors may exist, but this repository is the authoritative upstream implementation.

A lightweight, table-driven REST API framework for SAP ECC 6.0 and S/4HANA. This is the `main` branch, for ECC 6.0; S/4HANA systems use `s4hana`.

**Route REST requests to installed ABAP handlers through SM30 configuration.**

New handler behavior still requires ABAP implementation, activation and your normal change process. Configuration changes require appropriate authorization and review.

## Relationship to ABAPilot

This MIT-licensed repository publishes the table-driven REST framework behind the ABAPilot architecture. It is not the complete ABAPilot product or an MCP server implementation.

The [public ABAPilot MCP connector](https://github.com/NicoHern/abapilot-mcp) runs outside SAP and translates MCP tool calls into HTTPS requests to the separately licensed ABAPilot backend. Installing this framework does not install that backend, provide a backend licence or establish compatibility with the connector’s complete tool catalog.

Use this repository to inspect or adapt the REST routing approach. For the product’s installation requirements and supported configuration, see the [ABAPilot setup guide](https://crimsonconsultingsl.com/abapilot-abap-mcp-server-any-ide/). Framework examples and product deployments provide different evidence; verify the exact branch, revision and operations on your own system.

---

## Why?

If you've worked with REST APIs in ECC, you know the pain:

- **CL_REST_ROUTER** requires hardcoded routes, so a new route means a code change and a transport
- **SAP Gateway** follows a different service implementation and setup approach
- **RAP** requires S/4HANA

This framework takes a different approach: **routes are configuration, not code**.

| Endpoint path | HTTP method | Class | Method | Active |
|---------------|-------------|-------|--------|--------|
| `/ping` | GET | `ZCL_AGENT_PING` | `HANDLE_PING` | X |
| `/read_table_data` | POST | `ZCL_AGENT_READ` | `HANDLE_READ_TABLE_DATA` | X |
| `/hello` | POST | `ZCL_MY_HANDLER` | `HELLO` | X |

A route can point to any handler that is already installed. Adding a new handler or changing its logic remains a code change.

---

## Features

- **Dynamic routing**: path and HTTP method map to a class and method through a config table
- **ABAP implementation**: no SAP Gateway or BTP dependency for this routing approach. Repository code is MIT-licensed; SAP system and other software licence requirements remain separate.
- **ECC and S/4HANA branches**: choose the branch for your target system and validate it there. This repository does not publish a release-by-release test matrix.
- **Per-route on/off switch**: `IS_ACTIVE` disables a route without changing code
- **Security layer**: per-route authorization, a table denylist, an audit log and request checks. See [Security Considerations](#security-considerations)
- **SM30 maintenance**: standard SAP tooling

---

## Installation

### Choose your branch

| System | Branch | Why |
|--------|--------|-----|
| **ECC 6.0** | `main` | Avoids newer ABAP syntax |
| **S/4HANA** | `s4hana` | Uses `CONV string( )` for dictionary fields and ships the table maintenance objects |

If you hit a `SYNTAX_ERROR` dump like the one below on S/4HANA, you are on the wrong branch. `CONV` needs ABAP 7.40 or later, so `s4hana` will not compile on older ECC releases.

```
"LS_DD03P-FIELDNAME" is not type-compatible with formal parameter "IV_INPUT"
```

### Prerequisites

- SAP ECC 6.0 development or sandbox system; confirm release compatibility before use
- Developer access to create objects in a custom namespace
- [abapGit](https://abapgit.org/) installed

### Steps

1. **Clone via abapGit**

   ```
   Repository URL: https://github.com/NicoHern/abap-dynamic-rest
   Branch: main
   Package: ZAGENT (or your choice)
   ```

2. **Activate all objects**
   - Tables and table type: `ZAGENT_ENDPOINTS`, `ZAGENT_DENY`, `ZAGENT_AUDIT`, `ZAGENT_ENDPOINTS_TT`
   - Data elements: `ZDYN_ENDPOINT`, `ZDYN_HTTP_METHOD`
   - Interface: `ZIF_AGENT_HANDLER`
   - Classes: `ZCL_AGENT_AUTHZ`, `ZCL_AGENT_DISPATCHER`, `ZCL_AGENT_HANDLER_BASE`
   - Sample handlers: `ZCL_AGENT_PING`, `ZCL_AGENT_READ`

3. **Set up table maintenance** (required, this branch does not ship it)
   - Create authorization group `ZAGT` first (SE54 → Authorization Groups). Anyone who can change these tables can expose routes or lift the denylist, so grant `S_TABU_DIS` for `ZAGT` narrowly.
   - SE11 → table `ZAGENT_ENDPOINTS` → Utilities → Table Maintenance Generator
   - Authorization group: `ZAGT`
   - Function group: `ZAGENT_MAINT`
   - Maintenance type: one step, screen `0001`
   - Repeat for `ZAGENT_DENY` with function group `ZAGENT_DENY`, and assign `ZAGT` to `ZAGENT_AUDIT` in SE54
   - If you upgraded from an earlier version, regenerate the `ZAGENT_ENDPOINTS` dialog so SM30 shows the new fields

4. **Create the SICF node** (not shipped by abapGit on either branch)
   - Transaction: `SICF`
   - Path: `/sap/bc/zagent` (or your choice)
   - Handler list: `ZCL_AGENT_DISPATCHER`
   - Activate the service

5. **Register routes in SM30**
   - Table: `ZAGENT_ENDPOINTS`
   - One row per path and HTTP method; register only what you intend to expose

   | ENDPOINT_PATH | HTTP_METHOD | CLASS_NAME | METHOD_NAME | IS_ACTIVE |
   |---------------|-------------|------------|-------------|-----------|
   | /ping | GET | ZCL_AGENT_PING | HANDLE_PING | X |
   | /echo | POST | ZCL_AGENT_PING | HANDLE_ECHO | X |
   | /read_table_structure | POST | ZCL_AGENT_READ | HANDLE_READ_TABLE_STRUCTURE | X |
   | /read_table_data | POST | ZCL_AGENT_READ | HANDLE_READ_TABLE_DATA | X |
   | /stream_table_data | POST | ZCL_AGENT_READ | HANDLE_STREAM_TABLE_DATA | X |
   | /read_domain_values | POST | ZCL_AGENT_READ | HANDLE_READ_DOMAIN_VALUES | X |
   | /check_type_exists | POST | ZCL_AGENT_READ | HANDLE_CHECK_TYPE_EXISTS | X |

6. **Test**

   ```bash
   curl -u <user> "https://<host>:<port>/sap/bc/zagent/ping"

   curl -u <user> -X POST "https://<host>:<port>/sap/bc/zagent/read_table_structure" \
     -H "Content-Type: application/json" \
     -d '{"table_name": "T000"}'
   ```

---

## Quick Start

### 1. Create a handler class

```abap
CLASS zcl_my_handler DEFINITION PUBLIC
  INHERITING FROM zcl_agent_handler_base
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS hello.
ENDCLASS.

CLASS zcl_my_handler IMPLEMENTATION.
  METHOD hello.
    DATA: lv_name     TYPE string,
          lv_response TYPE string.

    lv_name = parse_json_param( iv_param_name = 'name' ).
    lv_name = escape_json_string( lv_name ).

    " Backticks keep the trailing blank; 'Hello ' would lose it in CONCATENATE
    CONCATENATE `{"message":"Hello ` lv_name `!"}` INTO lv_response.
    send_json( lv_response ).
  ENDMETHOD.
ENDCLASS.
```

### 2. Register the route (SM30)

| ENDPOINT_PATH | HTTP_METHOD | CLASS_NAME | METHOD_NAME | IS_ACTIVE |
|---------------|-------------|------------|-------------|-----------|
| /hello | POST | ZCL_MY_HANDLER | HELLO | X |

### 3. Call it

```bash
curl -u <user> -X POST "https://<host>:<port>/sap/bc/zagent/hello" \
  -H "Content-Type: application/json" \
  -d '{"name": "World"}'

# Response:
{"message":"Hello World!"}
```

Apply your normal authorization, testing and transport process to the handler class.

---

## Configuration Table

### ZAGENT_ENDPOINTS

| Field | Type | Description |
|-------|------|-------------|
| `ENDPOINT_PATH` | CHAR(100), key | Path after the SICF node, with leading slash (e.g. `/hello`) |
| `HTTP_METHOD` | CHAR(10), key | Exact method, e.g. GET or POST. There is no wildcard: add one row per method |
| `CLASS_NAME` | SEOCLSNAME | Handler class |
| `METHOD_NAME` | SEOCPDNAME | Handler method |
| `IS_ACTIVE` | BOOLEAN | X = enabled. Inactive rows are not loaded, so the path returns 404 |
| `AUTH_OBJECT` | XUOBJECT | Optional authorization object |
| `AUTH_FIELD` | XUFIELD | Optional authorization field to check |
| `AUTH_VALUE` | XUVAL | Value required for `AUTH_FIELD` |
| `AUTH_ACTVT` | ACTIV_AUTH | Activity required, e.g. `06` for a delete route |
| `DESCRIPTION` | CHAR255 | Free text |
| `CATEGORY` | CHAR20 | Used only to sort `GET_REGISTERED_ENDPOINTS` |
| `CREATED_BY`, `CREATED_ON`, `CHANGED_BY`, `CHANGED_ON` | | Not read by the dispatcher |

### How paths are matched

1. Exact match on path and HTTP method.
2. If that fails, the first active row with the same HTTP method whose path is contained anywhere in the request path. This fallback uses `CS`, which ignores case.

On this branch `ENDPOINT_PATH` has no lower-case flag, so SM30 stores `/hello` as `/HELLO`. Lower-case requests then miss the exact match and are routed by the fallback. `/ping` also answers `/myping` and `/pingtest`.

**Avoid paths that contain another registered path.** With `/read` and `/read_table` both registered, a fallback match may go to either handler, because the fallback takes the first match in load order.

### When changes take effect

Routes are cached in a static attribute of the dispatcher. A static attribute lasts only as long as the session, so a default stateless SICF service reloads the table on each request and SM30 changes apply straight away. `REFRESH_CACHE` only affects the session it runs in.

---

## Writing a Handler

The dispatcher creates the object, calls `INITIALIZE`, then calls your method dynamically with no parameters. A handler must therefore:

- inherit from `ZCL_AGENT_HANDLER_BASE` (implementing `ZIF_AGENT_HANDLER` alone is not enough)
- be a concrete class that can be instantiated without constructor parameters
- expose a public method with no mandatory parameters

The base class provides:

```abap
DATA: lv_value TYPE string,
      lv_num   TYPE i,
      lv_flag  TYPE abap_bool.

" Reading the JSON body
lv_value = parse_json_param( iv_param_name = 'field_name' ).
lv_value = parse_json_param( iv_param_name = 'table_name' iv_to_upper = abap_true ).
lv_num   = parse_json_int( iv_param_name = 'count' iv_default = 10 ).
lv_flag  = parse_json_bool( iv_param_name = 'active' ).

" Raw body and the request/response objects
lv_value = mv_body.
lv_value = mo_request->get_form_field( 'name' ).  " query parameter on a GET

" Writing the response
send_json( iv_json = lv_json_string ).                 " complete JSON document
send_success( iv_json = '"data":"value"' ).            " {"status":"success","data":"value"}
send_error( iv_message = 'Error message' iv_code = 400 ).

" Escape any text you embed in JSON yourself
lv_value = escape_json_string( lv_value ).
```

The `parse_json_*` helpers use regular expressions rather than a JSON parser. They return the first match anywhere in the body, do not understand nesting or arrays, and `parse_json_param` stops at the first `"` even if it is escaped. `parse_json_int` reads non-negative integers only.

---

## Included Handlers

All parameters are read from a JSON body, so use POST when a method takes parameters.

| Class | Method | Parameters | Returns |
|-------|--------|------------|---------|
| `ZCL_AGENT_PING` | `HANDLE_PING` | none | `message`, `timestamp`, `system`, `client` |
| `ZCL_AGENT_PING` | `HANDLE_ECHO` | `message` | `echo` |
| `ZCL_AGENT_READ` | `HANDLE_READ_TABLE_STRUCTURE` | `table_name` | `fields` |
| `ZCL_AGENT_READ` | `HANDLE_READ_TABLE_DATA` | `table_name`, `max_rows` (default 100, capped at 1000), `where` | `row_count`, `data` |
| `ZCL_AGENT_READ` | `HANDLE_STREAM_TABLE_DATA` | `table_name`, `package_size` (default 100, capped at 500), `offset` | `rows_in_chunk`, `data` |
| `ZCL_AGENT_READ` | `HANDLE_READ_DOMAIN_VALUES` | `domain` | `values` |
| `ZCL_AGENT_READ` | `HANDLE_CHECK_TYPE_EXISTS` | `type_name` | `exists`, `type_class` |

Limitations of the sample handlers:

- `HANDLE_READ_TABLE_STRUCTURE` writes `length` and `decimals` with leading zeros (e.g. `000003`), which strict JSON parsers reject.
- `HANDLE_STREAM_TABLE_DATA` has no `where` and no `ORDER BY`, and re-reads the skipped rows on each call. Row order between chunks is not guaranteed, so it is not reliable for complete extraction.
- `data` values are always returned as strings.
- There is no source code reading or syntax check handler in this repository.

---

## Architecture

```
HTTP request
  │
  ▼
SICF node /sap/bc/zagent → ZCL_AGENT_DISPATCHER
  1. Answer OPTIONS directly and set CORS headers
  2. Read the path after the SICF node
  3. Load active routes from ZAGENT_ENDPOINTS if not cached
  4. Match path and HTTP method (404 if none)
  5. Refuse non-GET requests that are not JSON (415, audited)
  6. ZCL_AGENT_AUTHZ=>CHECK_ROUTE (403 if refused, audited)
  7. Write the audit row
  8. CREATE OBJECT handler dynamically, then INITIALIZE
  9. Handler hook CHECK_AUTHORIZATION (403 if refused)
 10. CALL METHOD dynamically
  │
  ▼
Your handler class and method
```

---

## Comparison

| Feature | This framework | CL_REST_ROUTER | Gateway | RAP |
|---------|----------------|----------------|---------|-----|
| New route to an existing handler | SM30 entry | Code change | Model + code | CDS + behavior |
| New handler logic | ABAP class + transport | ABAP class + transport | Model + code + transport | CDS + behavior + transport |
| On/off flag per route | ✅ | ❌ | ❌ | ❌ |
| ECC 6.0 support | ✅ | ✅ | ✅ | ❌ |
| S/4HANA support | ✅ | ✅ | ✅ | ✅ |
| Licensing | MIT repository code; SAP licences separate | Check SAP agreement | Check SAP agreement | Check SAP agreement |

---

## Use Cases

- **AI integration**: expose selected SAP data and functions to external AI agents
- **Microservices**: lightweight REST APIs without Gateway overhead
- **Rapid prototyping**: try routes in a development system without a transport for each one
- **Legacy modernization**: add REST capabilities to ECC systems

---

## Security Considerations

Review these before exposing the service outside a development system.

### Route authorization

`ZCL_AGENT_AUTHZ=>CHECK_ROUTE` runs before the handler object is created, so a refused user runs no handler code.

| `AUTH_OBJECT` | `AUTH_FIELD` + `AUTH_VALUE` | `AUTH_ACTVT` | What is checked |
|---------------|-----------------------------|--------------|-----------------|
| blank | ignored | ignored | Nothing: open to any user who can log on to the SICF node |
| set | blank | blank | `ACTVT = 03` |
| set | blank | set | `ACTVT` = your value |
| set | set | blank | Your field = your value |
| set | set | set | Both |

`AUTH_FIELD` without `AUTH_VALUE` is refused as a configuration error. A field that does not exist on the object also fails the check. For anything more specific, redefine `CHECK_AUTHORIZATION` in your handler, which runs afterwards as an extra check.

### Table access in ZCL_AGENT_READ

`ZCL_AGENT_AUTHZ=>CHECK_TABLE_READ` applies two layers:

1. **Denylist:** no authorization overrides it. `USR02`, `USH02`, `RFCDES`, `SSF_PSE_D` and this framework's own tables are always denied. Add your own in `ZAGENT_DENY`; `TABNAME` accepts patterns such as `PA*`. A view is denied if any table behind it is denied.
2. **Table authorization:** it reads the table's authorization group from `TDDAT`, treating a table with no group as `&NC&`. The call is allowed with display access (`ACTVT = 03`) through `S_TABU_DIS` for that group or `S_TABU_NAM` for that table.

`HANDLE_READ_DOMAIN_VALUES` and `HANDLE_CHECK_TYPE_EXISTS` return dictionary metadata and have no authorization check.

### Dynamic WHERE clause

`HANDLE_READ_TABLE_DATA` passes the `where` parameter into a dynamic SELECT. `ZCL_AGENT_AUTHZ=>IS_WHERE_SAFE` refuses subqueries, which could otherwise read tables that were not authorized. Any other condition on the requested table is still allowed, so it is not field-level security.

Treat the sample handlers as examples, not a hardened API:

- Register these routes only for trusted internal consumers
- Add an allowlist of fields and operators before wider use
- Use a dedicated technical user with narrowly scoped authorizations

### Audit log

`ZAGENT_AUDIT` records each routed call: time, user, path, HTTP method, handler, table read, decision (`A` allowed, `D` denied), reason and client IP. Request and response bodies are not stored, and paths with no route are not logged. Behind a proxy, the client IP may be the proxy's address.

The row is committed before the handler runs, so it survives a rollback or a dump in the handler. If a custom handler calls `ZCL_AGENT_AUTHZ=>AUDIT`, do it before opening a database cursor or changing data, because it commits. Nothing purges the table, so schedule your own retention job.

### Cross-site requests

Non-GET requests must send `Content-Type: application/json` or get a 415. A cross-site HTML form cannot set that type, and a browser only sends a cross-site JSON request without credentials while `Access-Control-Allow-Origin` is `*`. This is a header check, not CSRF tokens. Keep GET routes free of changes to data, and if you narrow CORS to named origins with credentials, add token validation.

### CORS

Every response sends `Access-Control-Allow-Origin: *` and `OPTIONS` is answered with 200. Restrict this in `SET_CORS_HEADERS` if browsers should not call the service from any origin, and see [Cross-site requests](#cross-site-requests).

---

## Troubleshooting

| Symptom | Likely cause |
|---------|--------------|
| HTTP 404 `Unknown endpoint` | No active row for this path and HTTP method. Check the leading slash, the exact method and `IS_ACTIVE` |
| HTTP 404 from the ICF, not JSON | SICF node missing or not activated |
| Wrong handler answers | A registered path is contained in the request path; see [How paths are matched](#how-paths-are-matched) |
| HTTP 403 `Not authorized` | User fails the route check; see [Route authorization](#route-authorization) |
| HTTP 415 `Content-Type must be application/json` | A non-GET request without the JSON content type |
| HTTP 400 `Subqueries are not allowed in where` | The `where` parameter contains a subquery |
| HTTP 403 `Not authorized for table access` | Table is on the denylist, or user has no display authorization for its group (`S_TABU_DIS`) or the table itself (`S_TABU_NAM`). `ZAGENT_AUDIT` shows the refusal |
| HTTP 500 `Handler class not found` | Class is missing, inactive or cannot be instantiated |
| HTTP 500 `Handler error` | Method missing, has mandatory parameters, or the handler raised an exception. The dispatcher does not return the exception text, so debug the handler |
| HTTP 500 with `SYNTAX_ERROR` | Check ST22; if it mentions type compatibility, see [Choose your branch](#choose-your-branch) |
| SM30 says the table cannot be maintained | Table maintenance dialog not generated yet; see installation step 3 |

---

## Contributing

Issues and PRs welcome. Please:

1. Test on the branch you are changing, and on both ECC 6.0 and S/4HANA if the change applies to both
2. Keep dependencies at zero
3. Document any new configuration fields
4. Avoid ABAP 7.40+ syntax on `main`; `CONV string( )` belongs on `s4hana`

---

## License

MIT License. See [LICENSE](LICENSE).

---

## Author

**Nicolas Hernandez** — SAP Technical Consultant @ Crimson Consulting
- LinkedIn: [nicolas-hernandez-abap](https://www.linkedin.com/in/nicolas-hernandez-abap/)
- GitHub: [NicoHern](https://github.com/NicoHern)
- Email: nicolashernandez1987@gmail.com
- ABAPilot enquiries: [nicolashernandez@crimsonconsultingsl.com](mailto:nicolashernandez@crimsonconsultingsl.com)

Built in Valencia, Spain 🇪🇸

---

## Acknowledgments

- [abapGit](https://abapgit.org/) — for making ABAP open source possible
- The SAP Community — for feedback and encouragement
