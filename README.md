# RINA for Space
![image](/RINA_Logo.png "Rina for Space Logo")

![image](/SponsorLogo.png "Sponsor - AdaCore Logo")

## Table of Contents
- [Overview](#overview)
- [What is RINA for Space?](#what-is-rina-for-space)
- [Getting Started](#getting-started)
- [References](#references)
- [Credits](#credits)

## Overview

**RINA for Space** is a senior design capstone project developed at The Pennsylvania State University, Behrend College, with sponsorship from AdaCore through its GNAT Academic Program. This project explores the use of **Recursive InterNetwork Architecture (RINA)** and the **CCSDS Bundle Protocol** to address the challenges of delay-tolerant networking (DTN) in space communication systems.

Traditional IP-based communication protocols struggle to handle high-latency, intermittent, and resource-constrained environments such as deep space. This project simulates a modular, policy-driven alternative using RINA architecture, implemented in Ada, to support reliable and scalable interplanetary data exchange. The system is intended to lay the foundation for communication architectures used in future space missions, where resilience, autonomy, and adaptability are critical.


## What is RINA for Space?

RINA for Space is a prototype communication system that simulates key aspects of the **Recursive InterNetwork Architecture (RINA)** integrated with the **CCSDS Delay-Tolerant Networking (DTN) Bundle Protocol**. It organizes communication into recursive layers of Distributed IPC Facilities (DIFs) representing roles such as Earth, relay, and planetary segments.

Within each DIF, InterProcess Communication Processes (IPCPs) handle flow setup, routing logic, buffering, and retransmissions. The system leverages a Resource Information Base (RIB) to maintain internal state and supports pathfinding mechanisms to simulate routing decisions across the network. These capabilities demonstrate how applications can exchange data over long distances using store-and-forward principles within a flexible, policy-driven framework.

## Getting Started

## References

## Credits
