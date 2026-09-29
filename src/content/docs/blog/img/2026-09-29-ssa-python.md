---
authors:
- CyrilFerlicot
title: "Implementing a Static Single Assigment (SSA) in FAST-Python"
date:  2026-09-29
tags:
- FAST
- SSA
---

# Implementing the SSA of Python in FAST-Python

---

## What is SSA?

Static Single Assignment is a program representation where **every variable is assigned exactly once**. The same logical variable `x` is renamed into several *versions* `x_1`, `x_2`, ... — one per assignment — and each **use** points to the version that can actually provide its value.

The goal is to make *data flow* explicit in the AST: instead of "this `x` is assigned somewhere before", we get "this `x` is exactly the value produced by that assignment".

```python
x = 2        # x_1
print(x)     # uses x_1

x = 3        # x_2
print(x)     # uses x_2
```

The hard part starts when the value of a variable can come from **several** assignments depending on the control flow:

```python
if z > 3:
    x = 2     # x_1
else:
    x = 3     # x_2

print(x)      # uses phi(x_1, x_2)
```

After the merge of the two branches there is no single assignment that produced `x`: we say `x` has a **phi version** `phi(x_1, x_2)`, a virtual assignment stating "the value is `x_1` if we came from the first branch, `x_2` if we came from the second". An analysis consuming the SSA can then explore both possible values.

For us, the SSA is the third stage of the pipeline, sitting on top of the two previous stages:

**Import → Local resolution → CFG → SSA**

It has a hard prerequisite: we must know, for each use, *which declaration* it refers to (the [local resolution](https://github.com/moosetechnology/FAST-Python/blob/main/resources/doc/analysis.md#local-resolution) stage) and *which control-flow paths* exist (the CFG stage). If you have not read about the FAST CFG and its visitor, I strongly recommend the [*Visiting the control-flow graph* article](https://modularmoose.org/developers/fast-cfg/) first — this blog post is the direct continuation.

---

## Architecture: one visitor on top of the CFG

The whole transform lives in a single class, `FASTPythonSSAVisitor`. Its declaration shows the trick that makes it work: it combines **two** visiting abilities—

```smalltalk
FASTPythonVisitor << #FASTPythonSSAVisitor
	traits: 'FASTCFGTVisitor';
	slots: { #newVariablesMap. #localDeclarations };
	tag: 'CFG/LocalResolver/SSA';
	package: 'FAST-Python-Tools'
```

- the superclass `FASTPythonVisitor` brings the **language visitor** (the `visitFASTPyXxx:` methods generated from the model, through `FASTPyTVisitor`);
- the trait `FASTCFGTVisitor` brings the **CFG visitor** (the `visitCFGXxxBlock:` methods that walk the blocks of the CFG, plus the conditional-branch hooks).

So a single `accept:` can traverse the CFG (`visitCFGBlock:` and friends) and, when a block contains Python statements, dispatch back to the language-specific visit methods (`visitFASTPyAssignment:`, `visitFASTTCanBeVariable:`, ...).

The trait is generic and lives in the base FAST project (`FAST-Core-Tools`), which means any language model can reuse it the same way. It is the same trait-based composition idea as the local resolver, but applied to the CFG.

### The state

The visitor has only two instance variables:

- `localDeclarations` — an `IdentitySet` of every local declaration that received at least one SSA version while walking the entity. It is the source of the "current active versions" snapshot (see below).
- `newVariablesMap` — an `IdentityDictionary` used **only during the merge of conditional branches**, to accumulate, per branch, the versions that were created inside that branch.

And the entry point is a class-side convenience:

```smalltalk
FASTPythonSSAVisitor class >> resolve: aPythonEntity
    ^ self new resolve: aPythonEntity
```

### The three SSA value classes

The versions are not strings, they are **first-class objects** living in `FAST-Core-Tools` (again reusable across languages), and they are **Moose entities**: each version is added to the model, so it is persisted and exported with it.

- `FASTAbstractVariableVersionSSA` — abstract root. It holds the `localDeclaration` the version belongs to, and offers the queries we need once the SSA is built: `nodes` (the accesses directly linked to this version, the bidirectional opposite of `ssaVersion`), `localUses`, `readAccesses`, `writeAccesses` (restricted to this version), `name`, `isPhi`, `ssaVariables`.
- `FASTVariableVersionSSA` — a **basic version** for one assignment. It carries an integer `version` number, so `name` answers `x_1`, `x.y_1`...
- `FASTVariablePhiVersionSSA` — a **phi version**. It carries `choices`, the collection of versions being merged, so `name` answers `phi(x_1, x_2)`, and `isPhi` answers `true`.

```smalltalk
FASTVariableVersionSSA >> name
    ^ String streamContents: [ :s |
          s
              nextPutAll: self localDeclarationName;
              nextPut: $_;
              nextPutAll: version asString ]
```

```smalltalk
FASTVariablePhiVersionSSA >> name
    ^ 'phi(' , ($, join: (choices collect: #name)) , ')'

```

Nested merges flatten themselves: the phi's `addChoiceToPhi:` re-dispatches each choice into the receiving phi, so a phi inside a branch ultimately contributes its own choices to the outer phi.

### The links carried on the model

The SSA result is attached to the model through **two links** defined in FAST-Core-Tools on the trait `FASTTCanBeLocalDeclaration` (used by `FASTTEntity`, so available on every FAST entity):

- `entity ssaVersion` — on any *access* (read or write) to a variable, the version it is linked to. This is a bidirectional relation: each version exposes the accesses linked to it through its `nodes` collection.
- `declaration activeVersion` — on the local *declaration*, the current version at the current point of the traversal. This is the internal "state variable" of the algorithm: it is bumped at each assignment and replaced by a phi at each merge. It stays a plain attribute.

```smalltalk
FASTTCanBeLocalDeclaration >> activeVersion
    ^ self attributeAt: #activeVersion ifAbsent: [ nil ]
```

```smalltalk
FASTTCanBeLocalDeclaration >> ssaVersion
    <FMProperty: #ssaVersion type: #FASTAbstractVariableVersionSSA opposite: #nodes>
    ^ self attributeAt: #ssaVersion ifAbsent: [ nil ]
```

The SSA is then a traversal that, at every write, creates a version, adds it to the model, and sets it as `activeVersion`; at every read, copies `activeVersion` into `ssaVersion` of the access.

---

### The pipeline inside `resolve:`

`FASTPythonSSAVisitor>>#resolve:` is deliberately buildable in your own project step by step:

```smalltalk
FASTPythonSSAVisitor >> resolve: aFASTBehaviouralEntity
    "First we do a local resolution so that each entity is linked to its
     local declaration."
    FASTPythonLocalResolverVisitor resolve: aFASTBehaviouralEntity.

    "Then we build the CFG to know the different paths in the entity."
    cfg := aFASTBehaviouralEntity cfg.

    "And lastly, I can build the SSA now that I have the CFG and the
     local declarations."
    self visit: cfg
```

Three stages, three small lines. The local resolution is done *inside* the SSA visitor only for convenience; you can equally run it beforehand.

---

## Producing the SSA versions

### The core: `visitFASTTCanBeVariable:`

Just like the local resolver, the SSA logic is concentrated on the trait `FASTTCanBeVariable`, caught in a *single* visit method for every node kind that can be a variable (identifier, attribute access, subscript, walrus):

```smalltalk
FASTPythonSSAVisitor >> visitFASTTCanBeVariable: aTCanBeVariable

    aTCanBeVariable isVariableWriteAccess
        ifTrue: [ self handleNewAssignemntTo: aTCanBeVariable ]
        ifFalse: [
            aTCanBeVariable localDeclaration isNonLocalDeclaration ifFalse: [
                "Subscript is a special case since it is not directly assigned.
                 So the first read acts as a new declaration."
                (aTCanBeVariable isSubscript
                     and: [ aTCanBeVariable localDeclaration activeVersion isNil ])
                    ifTrue: [ self handleNewAssignemntTo: aTCanBeVariable ]
                    ifFalse: [ aTCanBeVariable ssaVersion: aTCanBeVariable localDeclaration activeVersion ] ] ].

    super visitFASTTCanBeVariable: aTCanBeVariable
```

The rule is a direct SSA translation of the local resolver's "write declares, read binds":

- **write access** → create a new version (`handleNewAssignemntTo:`);
- **read access** → copy the declaration's `activeVersion` into the access's `ssaVersion`;
- **read of an unresolved name** (`localDeclaration isNonLocalDeclaration`) → nothing. `print`, `len`, imported functions and variables have no declaration in the model, so they get no version (`ssaVersion` stays `nil`);
- **first read of a subscript** whose declaration has no version yet → *it behaves like a declaration*: the first `x[3]` read is treated as an assignment (mirroring the local-resolver convention), so its version is created there.

### Creating a version

```smalltalk
FASTPythonSSAVisitor >> handleNewAssignemntTo: aFASTEntity
    aFASTEntity ssaVersion: ((self createVariableVersionFor: aFASTEntity)
                 newVersionNumber;
                 yourself)
```

```smalltalk
FASTPythonSSAVisitor >> createVariableVersionFor: aFASTEntity
    | newSSA |
    newSSA := FASTVariableVersionSSA for: aFASTEntity.
    aFASTEntity mooseModel add: newSSA.
    aFASTEntity lastSSAVersion ifNotNil: [ :lastSSA | newSSA version: lastSSA version ].

    aFASTEntity localDeclaration activeVersion: newSSA.
    localDeclarations add: aFASTEntity localDeclaration.
    ^ newSSA
```

`FASTVariableVersionSSA for: aFASTEntity` records `aFASTEntity localDeclaration` as the declaration the version belongs to. Then:

1. the new version is **added to the model** — the versions are Moose entities, so this makes them persistent and exported with the model;
2. the version **number** is seeded from `lastSSAVersion` — the active version of the *previous* declaration of the same name, chased through the local-resolver shadowing chain: it is what makes `x_1` then `x_2` even when an unrelated `def x():` was declared in between;
3. the new version becomes the `activeVersion` of the declaration;
4. the declaration is added to `localDeclarations` (so the "snapshot" of active versions, used for the branches, sees it);
5. `newVersionNumber` bumps the integer — `version := version + 1` — and the result is stored as the write access's `ssaVersion`.

```smalltalk
FASTPyEntity >> lastSSAVersion
    "scrolling the shadowing chain of the declaration to find a previous version"
    | entity |
    entity := self localDeclaration.
    [ entity isNotNil ] whileTrue: [
        entity activeVersion ifNotNil: [ :version | ^ version ].
        entity := entity shadowing ].
    ^ nil
```

---

## Reordering the visits: the order matters for SSA

The generated visitor visits the children of a node in *metamodel* order. For the SSA, that order is often wrong because **a read must be resolved against the versions before the node, and a write must only bump the version after its right-hand side has been read**. We overwrite the generated visit methods in four places.

### Assignments: the right side comes first

This is the most important one. In the metamodel, the `left` of an assignment comes before its `right`. For the SSA we must invert:

```smalltalk
visitFASTPyAssignment: anAssignment
    "Visiting the right field before the left because the variables at the
     right should be linked to the previous assignment."
    self visitFASTPyTAssignable: anAssignment.
    self visitFASTPyExpression: anAssignment.

    self visitEntity: anAssignment right.
    self visitEntity: anAssignment left.
    self visitEntity: anAssignment type
```

Consider `x = x + 1`. In metamodel order we would visit the left `x` first: it would create `x_2` and set it as `activeVersion`, and *then* the `x` in `x + 1` would read — wrongly — the new `x_2`. By visiting `right` before `left`, the read sees `x_1` and the write bumps afterwards.

### Function definitions: parameters before the body

Parameters are declarations that the body reads, so they have to be visited before the body:

```smalltalk
visitFASTPyFunctionDefinition: aFunctionDefinition
    "Reordering so parameters are visited before the statements, in order
     to have all the declarations."
    self visitFASTTWithParameters: aFunctionDefinition.
    self visitEntity: aFunctionDefinition returnType.
    self visitFASTPyTDefinition: aFunctionDefinition.
    self visitFASTPyTWithTypeParameters: aFunctionDefinition.
    self visitFASTPyStatement: aFunctionDefinition

visitFASTPyMethodDefinition: aMethodDefinition
    "We resolve in the same way as functions."
    self visitFASTPyFunctionDefinition: aMethodDefinition
```

### Definitions rebuild their own CFG

A function definition appearing as a statement of the module is *inside* the module CFG. But the function's body is a separate control-flow world. When we reach a definition, we therefore **rebuild a fresh CFG and restart the traversal on it**, so each definition is turned into its own SSA form:

```smalltalk
visitFASTPyTDefinition: aTDefinition
    "We need to redo the CFG for definitions."
    | cfg |
    self visitFASTTNamedEntity: aTDefinition.
    self visitCollection: aTDefinition decorators.

    cfg := aTDefinition cfg.
    self visit: cfg
```

---

## Building the phi versions

Where do phi versions come from? The CFG visitor. When `FASTCFGTVisitor` visits a conditional block, it visits **all the branches before the merge**, and during that it calls **four hooks** you can override. Their default implementation in the trait is empty:

```smalltalk
preConditionalsBranchesVisitOf: aConditionalBlock          "before the first branch"
preConditionalsBranchVisitOf: block conditional: aConditionalBlock   "before each branch"
postConditionalsBranchVisitOf: block conditional: aConditionalBlock  "after each branch"
postConditionalsBranchesVisitOf: aConditionalBlock         "after all branches, before the merge"
```

That is exactly the machinery we fill in to build phis. The idea is a three-step protocol:

1. **before the branches**: snapshot the active versions;
2. **after each branch**: record which *new* versions were created by that branch;
3. **after all branches**: merge, per variable, one version per branch (plus the snapshot version for branches that did not assign it) into a phi.

### Step 1 — snapshot before the branches

```smalltalk
FASTPythonSSAVisitor >> preConditionalsBranchesVisitOf: aConditionalBlock
    "When we start to visit the branches of a conditional, we create an
     entry for this conditional in the new variables map and we save the
     active versions present before visiting the children."
    newVariablesMap at: aConditionalBlock put: (IdentityDictionary with: #previousVariables -> self currentActiveVersions)
```

`currentActiveVersions` is a snapshot of every declaration's active version at this point:

```smalltalk
currentActiveVersions
    ^ localDeclarations collect: #activeVersion
```

### Step 2 — record the new versions of each branch

```smalltalk
FASTPythonSSAVisitor >> postConditionalsBranchVisitOf: firstBranchBlock conditional: aConditionalBlock
    "Once we visited a conditional branch, we save the current active
     versions for this branch."
    | mapForConditional |
    mapForConditional := newVariablesMap at: aConditionalBlock.
    mapForConditional at: firstBranchBlock put: (self currentActiveVersions difference: (mapForConditional at: #previousVariables))
```

The `difference` is atomic: subtracting the snapshot (`#previousVariables`) from the current active versions leaves exactly the versions **created inside this branch** (the ones that were not there before). We store them per branch block.

### Step 3 — build the phis and merge

```smalltalk
FASTPythonSSAVisitor >> postConditionalsBranchesVisitOf: aConditionalBlock
    "Once we are done visiting a conditional block we can clean the map and build the Phi variables."

    | mapForConditional previousVariables |
    mapForConditional := newVariablesMap at: aConditionalBlock.
    previousVariables := mapForConditional at: #previousVariables.
    mapForConditional removeKey: #previousVariables.
    self producePhiVersionBasedOnPreviousVariables: previousVariables andNewVariableMap: mapForConditional.
    newVariablesMap removeKey: aConditionalBlock
```

The heavy lifting is in `producePhiVersionBasedOnPreviousVariables:andNewVariableMap:`:

```smalltalk
FASTPythonSSAVisitor >> producePhiVersionBasedOnPreviousVariables: previousVariables andNewVariableMap: newVariablesMap
    | declarationsToMerge |
    declarationsToMerge := newVariablesMap values flatten collectAsSet: #localDeclaration.

    declarationsToMerge do: [ :localDeclaration |
        | allVersions |
        "First we collect all the versions to use to produce a new Phi version
         for the current local declaration."
        allVersions := Set new.
        newVariablesMap valuesDo: [ :variables |
            variables
                detect: [ :variable | variable localDeclaration = localDeclaration ]
                ifFound: [ :variable | allVersions add: variable ] ].

        "If not all branches create a new version, we need to add the previous
         version also to the list of variables for the Phi variable. Note that
         it is possible it did not exist before."
        allVersions size = newVariablesMap size ifFalse: [
            previousVariables
                detect: [ :variable | variable localDeclaration = localDeclaration ]
                ifFound: [ :variable | allVersions add: variable ] ].

        self createPhiVersionFor: localDeclaration versions: allVersions ]
```

Decoding it:

1. `declarationsToMerge` — the set of *distinct local declarations* that received at least one new version in some branch;
2. for each such declaration, walk the branch map and gather the **one version per branch** that belongs to it;
3. **if not every branch created a version** for it, a branch may fall straight through with the *old* value — so we also merge in the version that was active **before** the conditional (from the `previousVariables` snapshot), if it existed;
4. `createPhiVersionFor:versions:` then decides.

```smalltalk
FASTPythonSSAVisitor >> createPhiVersionFor: localDeclaration versions: allVersions
    "No need to add it to the versions since it was already added by the
     previous versions."
    | phiVersion |
    allVersions size < 2 ifTrue: [ ^ self ]. "No phi if we do not have at least 2 versions"

    phiVersion := FASTVariablePhiVersionSSA for: allVersions.
    allVersions anyOne mooseModel add: phiVersion.
    localDeclaration activeVersion: phiVersion.
    ^ phiVersion
```

Two guard rails:

- if a single candidate version remains (e.g. only one branch assigns, and merging the previous version produces a single version) there is **nothing to merge**, so no phi is created — the single version just stays active;
- otherwise a `FASTVariablePhiVersionSSA` is created with the candidate versions as `choices`, and becomes the new `activeVersion` of the declaration — so the reads right after the merge pick it up via the normal "copy `activeVersion` into `ssaVersion`" rule.

No need to "attach" anything else: the phi inherits from the version classes, so it knows its `localDeclaration` and exposes `localUses`, `readAccesses`, `writeAccesses` restricted to the merged versions.

### Traced example

```python
if y < 2:
    x = 4     # x_1
else:
    x = 5     # x_2

print(x)      # phi(x_1, x_2)
```

Walked by the visitor:

1. `preConditionalsBranchesVisitOf:` → `previousVariables` snapshot is empty (no version yet).
2. `then` branch: `x = 4` → `handleNewAssignemntTo:` creates `x_1`, becomes active.
3. `postConditionalsBranchVisitOf:` → branch recorded with `{x_1}`.
4. `else` branch: `x = 5` → creates `x_2`, becomes active.
5. `postConditionalsBranchVisitOf:` → branch recorded with `{x_2}`.
6. `postConditionalsBranchesVisitOf:` → declaration merged: `{x_1}` from branch 1, `{x_2}` from branch 2 → both branches assigned, so no previous version added → `allVersions = {x_1, x_2}` → phi created, becomes active.
7. `print(x)` is read after the merge → `ssaVersion := activeVersion` → `phi(x_1, x_2)`. The use is linked to both possible values.

And the case where a branch does **not** assign the variable:

```python
x = 3       # x_1
if y < 2:
    function()   # no assignment of x
else:
    x = 4   # x_2

print(x)    # phi(x_1, x_2)
```

Step 6 here: branch 2 introduced `x_2` only, and branch 1 introduced none for `x` → `allVersions` starts as `{x_2}`, its size (1) is *not* the number of branches (2) → we add the previous version `x_1` from the snapshot → `allVersions = {x_1, x_2}` → phi. Correct: if the first branch is taken, `x` still holds the old value.

If the assignment happens in a single branch and no previous version exists, then `allVersions` ends up with a single element → **no phi**, and the reads just use that single version — matching the (correct) semantics that the variable is simply that assignment in all reachable paths.

---

## Querying the result

Once the SSA is built, the model can be queried directly:

- `access ssaVersion` — the version of the access: a `FASTVariableVersionSSA` or a `FASTVariablePhiVersionSSA` (or `nil` for unresolved names and non-local declarations).
- `access ssaName` — its pretty name, e.g. `x_1`, `x.y_2`, `phi(x_1, x_2)`.
- `version nodes` — the accesses directly linked to this version through `ssaVersion` (the bidirectional opposite).
- `version localUses` / `version readAccesses` / `version writeAccesses` — the accesses **for this specific version** (in the phi case, over all its choices).
- `model allSSAVersions` / `model allResolvedVariableVersions` — all the versions of the model, the latter without duplicates.
- `variable allSSAVersions` / `variable allSSABasicVersions` — all versions of one variable, the latter without the phis.

```smalltalk
(printCall arguments first) ssaVersion name.          "phi(x_1, x_2)"
(printCall arguments first) ssaVersion localUses size.
```

The full API is documented in the [*Exploiting the SSA* section of `analysis.md`](https://github.com/moosetechnology/FAST-Python/blob/main/resources/doc/analysis.md#exploiting-the-ssa).

---

## Known limits

The SSA inherits the limitations of the two stages it sits on:

- **Instance variables** (`self.x`) cannot be treated correctly: there is no declaration of them, and we do not know the order in which methods are invoked.
- **Subscripts are matched by their source code**, so `x[a]` and `x[b]` are conflated — and the whole "first read acts as a declaration" convention is a workaround, not a real answer.
- **Non-local declarations get no version**: unresolved and imported names are invisible to the SSA.
- **Attribute access chains** are not tracked (`a = x.y` then `a.z` is not linked to `x.y.z`).

You will find these documented in the [*Limitations* section](https://github.com/moosetechnology/FAST-Python/blob/main/resources/doc/analysis.md#limitations-of-local-resolver-and-ssa).

---

## Quick start

```smalltalk
"Import"
model := FASTPythonImporter parseFile: aFile.

"Full pipeline: local resolution is done inside"
FASTPythonSSAVisitor resolve: model allFunctionDefinitions first.

"Or explicitly, step by step"
model := FASTPythonImporter parseFile: aFile.
FASTPythonLocalResolverVisitor resolve: model module.
model allFunctionDefinitions first cfg.
FASTPythonSSAVisitor resolve: model allFunctionDefinitions first.
```

The SSA requires **Python 3** scoping, like the local resolver it builds on.

---

## Advice for implementing SSA in your own FAST project

1. **Reuse the generic machinery.** `FASTCFGTVisitor`, `FASTVariableVersionSSA`, `FASTVariablePhiVersionSSA` and the version queries live in the base FAST project (`FAST-Core-Tools`). You should only have to write the language-specific part: the `visitX:` rules for your nodes and the branch hooks.
2. **The write/read rule is your whole core.** One visit on the trait "can be a variable" (read → copy active version, write → create version) is enough, provided the local resolution already linked uses to declarations.
3. **Drive phis from the CFG hooks, not by scanning.** The four `pre/postConditionals*` hooks give you the branch boundaries for free. Snapshot before, record per branch, merge after — that protocol is generic.
4. **Order your visits so reads see the old version.** The metamodel order is a trap: reorder assignments (right before left), functions (parameters before body), and anything that mixes declaration and use.
5. **Give each access a version and each declaration a "current version" attribute.** A `ssaVersion` relation (bidirectional with `nodes`) and an `activeVersion` attribute on `FASTTCanBeLocalDeclaration` were enough. Make the versions Moose entities and add them to the model so they are persisted and exported.
6. **Make the versions first-class objects, not strings.** Give them a `localDeclaration`, `name`, and `ssaVariables`/`choices`; the query API (`localUses`, `readAccesses`, ...) then comes almost for free on the abstract root.
7. **Handle your "no declaration" case explicitly.** In Python, unresolved/imported names simply get no version. Decide early what your language's equivalent is (a non-local placeholder, an explicit global scope...).

The `FAST-Python` sources you will want to look at: `FASTPythonSSAVisitor` (this post is a guided read of it), the `FASTCFGTVisitor` trait and the `FAST*VersionSSA` classes in `FAST-Core-Tools`, and the SSA tests in `FASTPythonSSATest`.

---

## Moving this implementation to FAST

This implementation currently lives in `FAST-Python`, but almost everything in it could move to the base FAST project in the future.

The language-specific part is surprisingly thin. Given a **CFG**, a **local resolution**, and a model whose variables use the `FASTTCanBeVariable` trait, this SSA implementation gives you almost everything for free:

- the version classes (`FASTVariableVersionSSA`, `FASTVariablePhiVersionSSA`) and the `FASTCFGTVisitor` trait already live in `FAST-Core-Tools`;
- the write/read rule is a single visit method on `FASTTCanBeVariable`;
- the phi building runs on the generic CFG hooks.

So, for a new language, there is almost nothing more to do: mostly override the few `visitX:` methods where the *metamodel order* is wrong for SSA — assignments (right before left), functions (parameters before body), and definitions (rebuild their own CFG) — to reorder the visits.