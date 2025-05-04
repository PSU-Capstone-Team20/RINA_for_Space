# RINA for Space
![image](/RINA_Logo.png "Rina for Space Logo")

![image](/SponsorLogo.png "Sponsor - AdaCore Logo")

## Table of Contents
- [Overview](#overview)
- [What is RINA for Space?](#what-is-rina-for-space)
- [Getting Started](#getting-started)
- [Troubleshooting](#troubleshooting)
- [References](#references)
- [Credits](#credits)

## Overview

**RINA for Space** is a senior design capstone project developed at [The Pennsylvania State University, Behrend College](https://behrend.psu.edu/), with sponsorship from [AdaCore](https://www.adacore.com/) through its [GNAT Academic Program](https://github.com/GNAT-Academic-Program). This project explores the use of **Recursive InterNetwork Architecture (RINA)** and the **CCSDS Bundle Protocol** to address the challenges of delay-tolerant networking (DTN) in space communication systems.

Traditional IP-based communication protocols struggle to handle high-latency, intermittent, and resource-constrained environments such as deep space. This project simulates a modular, policy-driven alternative using RINA architecture, implemented in Ada, to support reliable and scalable interplanetary data exchange. The system is intended to lay the foundation for communication architectures used in future space missions, where resilience, autonomy, and adaptability are critical.


## What is RINA for Space?

RINA for Space is a prototype communication system that simulates key aspects of the **Recursive InterNetwork Architecture (RINA)** integrated with the **CCSDS Delay-Tolerant Networking (DTN) Bundle Protocol**. It organizes communication into recursive layers of Distributed IPC Facilities (DIFs) representing roles such as Earth, relay, and planetary segments.

Within each DIF, InterProcess Communication Processes (IPCPs) handle flow setup, routing logic, buffering, and retransmissions. The system leverages a Resource Information Base (RIB) to maintain internal state and supports pathfinding mechanisms to simulate routing decisions across the network. These capabilities demonstrate how applications can exchange data over long distances using store-and-forward principles within a flexible, policy-driven framework. Official project summary, poster, and demo video are available on the [Penn State Behrend Senior Design Showcase](https://sites.psu.edu/behrendseniordesign/2025/04/26/rina-for-space/).

![image](/demo.gif "System simulation image") 

## Getting Started
### Prerequisites
Before building the project, ensure the following tools are installed:
* [Alire](https://alire.ada.dev/) – Ada package manager used to manage dependencies and build the project
* GNAT Ada Compiler – Installed automatically via Alire
* [Git](https://git-scm.com/) – To clone the repository
* [Visual Studio Code](https://code.visualstudio.com/) - Recommended IDE
> 🛠️ Alire is available for Windows, macOS (x86), and Linux. Visit https://alire.ada.dev for download and installation instructions.

### Installation
Clone this repository, change directory to the repository:
```
git clone https://github.com/PSU-Capstone-Team20/RINA_for_Space.git
cd RINA_for_Space
```
Install dependencies and build the project:
``` 
alr build
```
Run the project and launch and display simulation in the terminal:
``` 
alr run
```
### Running Tests
This project includes unit and integration tests written using Ada assertions to validate the behavior of core components such as IPCPs, DIFs, and RIB. To run the tests: Ensure the project is built after ```alr build```.

Run a specific test executable located in the bin/ directory. For example:
```
bin/test-ipcp
```
Each test will print output and assertion messages to the console. If an assertion fails, the message will indicate which component or behavior did not meet expectations.
> 🧪 Tip: You can explore the tests/ directory to view available test files and understand the test coverage.

## Troubleshooting
❓ alr command not found : Make sure Alire is installed and available in your system’s PATH.
* On Unix/macOS: Add the export line provided by the Alire installer to your .bashrc, .zshrc, or .profile.
* On Windows: Ensure Alire’s install path is added to the system PATH environment variable.

❓ alr build fails with dependency errors
* Try updating Alire and re-resolving dependencies:
```
alr update
alr build
```
❓ File permissions error on test binaries (Linux/macOS) : Ensure to make test files executable:
```
chmod +x bin/*
```

## References

#### Pouzin Society - RINA
[1] PSOC, “RINA, Recursive Internetwork Architecture,” Nov. 14, 2019. https://www.open-root.eu/IMG/pdf/rina-leaflet_20191115_en.pdf

#### AdaCore Company Site
[2] “AdaCore,” AdaCore, 2024. https://www.adacore.com/

#### Patterns in Network Architecture - John Day
[3] J. Day, Patterns in Network Architecture. Pearson Education India, 2007.

#### Is the Internet an Unfinished Demo? Meet RINA! Research Paper
[4] E. Trouva et al., “IS THE INTERNET AN UNFINISHED DEMO? MEET RINA!,” 2010. Accessed: Feb. 26, 2025. https://www.cs.bu.edu/fac/matta/Papers/Internet-unfinished-demo-Meet-RINA.pdf

#### CCSDS Public Documents - Communication protocols
[5] Consultative Committee for Space Data Systems, “CCSDS Public Documents,” CCSDS. [Online]. Available: https://public.ccsds.org/default.aspx

## Credits

This project was made possible through the collaborative efforts of our senior design team, with guidance from our mentor at AdaCore and faculty advisors.

Team Members – Penn State Behrend, Class of 2025
* [Dominic Pasquarelli](https://github.com/dominic24515)
* [Glenn Bartkowiak](https://github.com/Glennkabob)
* [Emily Kan](https://github.com/emilyyankan)
* [Hyelin Lee](https://github.com/mashimelo)

Sponsor Project Mentor
* [Olivier Henley](https://github.com/ohenley)

Faculty Advisor
* Naseem Ibrahim, Ph.D. - Chair, Computer Science and Software Engineering. Associate Professor, Computer Science and Software Engineering

* Pulin Agrawal, Ph.D. - Assistant Professor, Computer Science and Software Engineering

* Chen Cao, Ph.D. - Assistant Professor, Computer Science and Software Engineering

