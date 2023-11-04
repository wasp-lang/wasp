korisnikova definicija queryja: src/queries.js

Ciljevi:

- Zelimo li da gtd na frontendu i backendu vodi na implementaciju koju je definirao korisnik (ovo je laz)?
- Zelimo li da tip na frontendu bude tocan tip (tip generiranog rpc poziva, tip koji tocno opisuje runtime fenkcije)?
- Zelimo li da tip na backendu bude tocan tip (tip generiranog dekoratora na backendu, tip koji tocno opisuje runtime funkcije)?
- Zelimo li da su importi isti cak i ako API bude razlicit?

## Rjesenje 1

To mozemo postici tako da:

- Kazemo korisniku da omota svoju implementaciju queryja u nas hook koji vraca tocan tip
- Ucinimo tip na frontendu istim ko na serveru (izbacimo query cache key) zato sto imamo samo jedno mjesto na kojem mozemo zakacit tip na query, ali dva mjesta koja ga moraju koristit
- note: korisnik bi sada _morao_ omotavati svoje queryje, assignement tipa vise ne bi bio izborna stvar

## Opcija 1

GTD na klijentu pokazuje na generirani rpc poziv na klijentu
GTD na serveru pokazuje na generirani poziv queryja (dekorator) na serveru

properties:

- nije potreban post processing da se projekt uspjesno koristi
- import za server i klijent ne smije biti isti string

## Opcija 2

GTD na klijentu pokazuje na korisnikov query u src/queries.js
GTD na serveru pokazuje na korisnikov query u src/queries.js

properties:

- potrebno je mijenjanje importa da se projekt koristi

## Martinova ideja

Generirani library kod u @wasp/queries/getSomething u runtimeu odlucuje hoce li koristit klijentsku ili serversku implementaciju queryja.
Korisnikov kod i dalje na gtd pokazuje na definiciju queryja koju je korisnik napisao (omotanu u hook) - vjerojatno bismo to postigli pomocu tsconfiga.
U tom bismo slucaju trebali prilagoditi tsconfig prije pokretanja korisnikovog koda u Wasp datoteci.
Mozemo izbjec runtime odlucivanje o implementaciji koju koristimo ako znamo koji fileovi su na serveru a koji na klijentu

file://./MainPage.tsx
http://google.com

## Najgluplje rjesenje

- dva importa koji se resolveaju u razlicite lokacije za server i za klijent (tipovi su razliciti)
- svaki ide na svoju implementaciju u node_modules SDK kodu
- jedini nacin da ovdje dobijemo GTD je pisanje TS LS plugina koji ce importe
  resolveat na Wasp file uz tocne tipove, sto je cudno jer iz typescripta vodimo
  na Wasp (al ok, radili smo mi i gore stvar)
- istrazit jel se moze napravit typescript lsp plugin

## Sljedece najgluplje rjesenje

- Importi se resolveaju u user kod na isti file koji je napisao korisnik, to postignemo s konfiguarcijom projekta (path resolution/aliases)
- Mozemo odlucit zelimo li iste ili razlicite import pathove (vjerojatno razlicite jer ljude zbunjuje)
- Mogli bismo imat tocne relativne import pathove (import getTask from
  '../queries.ts'). Ovo zadnje bi znacilo da svoj _pravi_ kod ne moze nikak
  importat, a to nam je predivlje.
- Tipovi moraju bit jednaki. Odaberemo tip tako da wrapper (koji bismo morali dodat) njega ima kao povratnu vrijednost.
- Problem kod servera je sto operacije nekad trebaju korisnika kojeg ne mogu same znati.
- U tom slucaju, bolje je da na serveru imamo pogresan runtime tip nego na
  klijentu. Normalan poziv operacije koja ovisi o Useru na serveru bi bacio runtime
  exception, a mogli bismo ju pozvati s nekim dodatnim "hookom"
  ```typescript
  getTask.callQuery(user);
  callQuery(getTask, user);
  getTask.withUser(user);
  getTask.withContext(context);
  ```

```typescript
const getTask = defineQuery("getTask", () => {});
```

## Ideje

- Treba li nam znanje o tome koji su fileovi na serveru ili klijentu
- mozda ne trebamo uopce generirati istinite (generirane) implementacije queryja
  i akcija u library kodu (ako gtd pokazuje na korisnikov), a u generiranom kodu
  (.wasp/out) presretnemo rezoluciju drugim node_modules/@wasp folderom koji
  sadrzi tocne implementacije (jedan u .wasp/out/server, drugi u
  .wasp/out/web-app)

## Sastanak u srijedu

```typescript
type Args = string;
type Context = object; // sadrzi usera
type Task = object;

// client

import getTask from "@wasp/queries/getTask";

// opcija 1
// ovo ne odgovara stvarnom tipu u runtimeu
getTask: (args: Args, context: Context) => Task;

// opcija 2
// ovo odgovara stvarnom tipu u runtimeu
getTask: (args: Args) => Task;

// opcija 3
// ovo odgovara stvarnom tipu u runtimeu
getTask: (args: Args) => Task;

// server

import getTask from "@wasp/queries/getTask";

// opcija 1
// ovo odgovara stvarnom tipu u runtimeu
getTask: (args: Args, context: Context) => Task;

// opcija 2
// ovo ne odgovara stvarnom tipu u runtimeu
getTask: (args: Args) => Task;

// opcija 3
// ovo ne odgovara stvarnom tipu u runtimeu
// ali baca exception jer nema konteksta
getTask: (args: Args) => Task;

// kak cemo ju koristit
getTask.callQuery();
callQuery(getTask);
getTask.withUser();
```
