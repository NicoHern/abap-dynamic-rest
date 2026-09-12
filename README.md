# ABAP Dynamic REST — Canonical Repository

**Canonical source:** [NicoHern/abap-dynamic-rest](https://github.com/NicoHern/abap-dynamic-rest), created and maintained by [Nicolas Hernandez](https://github.com/NicoHern). Forks and mirrors may exist, but this repository is the authoritative upstream implementation.

A lightweight, table-driven REST API framework for SAP ECC 6.0 and S/4HANA.

**Route REST requests to installed ABAP handlers through SM30 configuration.**

New handler behavior still requires ABAP implementation, activation and your normal change process. Configuration changes require appropriate authorization and review.

## Relationship to ABAPilot

This MIT-licensed repository publishes the table-driven REST framework behind the ABAPilot architecture. It is not the complete ABAPilot product or an MCP server implementation.

The [public ABAPilot MCP connector](https://github.com/NicoHern/abapilot-mcp) runs outside SAP and translates MCP tool calls into HTTPS requests to the separately licensed ABAPilot backend. Installing this framework does not install that backend, provide a backend licence or establish compatibility with the connector’s complete tool catalog.

Use this repository to inspect or adapt the REST routing approach. For the product’s installation requirements and supported configuration, see the [ABAPilot setup guide](https://crimsonconsultingsl.com/abapilot-abap-mcp-server-any-ide/). Framework examples and product deployments provide different evidence; verify the exact branch, revision and operations on your own system.

---

## Why?

If you've worked with REST APIs in ECC, you know the pain:

- **CL_REST_ROUTER** requires hardcoded routes — new endpoint = code change + transport
- **SAP Gateway** requires significant setup
- **RAP** requires S/4HANA

This connector takes a different approach: **endpoints are configuration, not code**.
```
┌─────────────────────────────────────────────────────────────────┐
│  ZAGENT_ENDPOINTS (SM30)                                        │
├──────────────┬─────────────────────┬───────────────┬────────────┤
│ Endpoint     │ Handler Class       │ Method        │ Active     │
├──────────────┼─────────────────────┼───────────────┼────────────┤
│ /query       │ ZCL_AGENT_QUERY     │ EXECUTE       │ X          │
