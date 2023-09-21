---
layout: page
background: '/img/bg-wiki.jpg'
---

# VerveineC-Cpp

VerveineC-Cpp (VerveineC in short) is a tool written in java that create a *json* or a *mse* file from C/C++ source code.

> VerveineC is not as stable as VerveineJ, do not hesitate to contact us in case of problem

## User Documentation

## Developer Documentation

### Installation

#### Download Eclipse

To install VerveineC, you need to clone it form [VerveineC github repositiory](https://github.com/moosetechnology/VerveineC-Cpp).
We advice you to contact us to know which branch is the best for now (probably `famix-7`).

```bash
# https
git clone https://github.com/moosetechnology/VerveineC-Cpp.git

# ssh
git clone git@github.com:moosetechnology/VerveineC-Cpp.git
```

VerveineC is *in reality* an eclipse plugin that will run in headless model.
Thus, you will have to download eclipse (eclipse Version: 2022-06 should work perfectly).

Then, you'll have to install the RCP Plug-In of eclipse and CDT.

#### Install RCP plug-in

To install the *RCP Plug-In*, in eclipse:

- *Help → Install New Software... → Choose The Eclipse Project Updates in field Work With → Filter for RCP Plug-In*.
- Select the *Eclipse Plug-in Development Environment*
- Perform the full install

### Potential issue

Your eclipse might not have the exact same plugin preinstalled.
In such a situation, you can search for equivalent plugin preinstalled, or install the plugin.

For instance, we had to change:

| source                  | target              |
| ----------------------- | ------------------- |
| `org.junit.jupiter.api` | `junit-jupiter-api` |

### Export the plug-in

To export the plugin, 

- `File` > `Export...` > `Plug-in Development` > `Deployable plug-ins and fragments` > Next
- Select the verveineC plugin and the root directory of your eclipse then *Finish*
