# Route: NoC Exploration in Chisel 
[![Scala Test CI](https://github.com/tjarker/route/actions/workflows/scala.yml/badge.svg)](https://github.com/tjarker/route/actions/workflows/scala.yml)

# Simple NoC Router

```scala
val params = SimpleNocParams[UInt](
  nx = 2,
  ny = 2,
  payloadGen = () => UInt(32.W),
  bufferFactory = SimpleBuffer,
  arbiterFactory = ChiselArbiter,
  routingPolicy = XYRouting
)
NocBuilder.build(
  portType = new SimpleRouterPort[UInt](Local)(params), 
  topologyBuilder = new MeshBuilder(params.nx, params.ny), 
  routerFactory = new SimpleRouterFactory[UInt](params)
)
```

## Synthesis Results Sky130

forward path delay = `valid` + `data` from `routerA.buffer` to `routerB.buffer` (`routerA.routing` + `routerA.arbitration` + `routerB.buffer_write`)

backward path delay = `ready` from `routerB.buffer` back to `routerA.buffer` (`routerB.buffer_status` + selecting correct buffer to handshake with in `routerA`)


| Config | Area | forward path delay (routerA + routerB) | backward path delay (routerB + routerA) | max link delay @100MHz |
|--------|------|------|------|------|
| x-y-routing, chisel-queue, chisel-arbiter, 8-bit | 30509.3 um² | 4.5ns + 1ns | 2ns + 3ns | 4.5ns |


