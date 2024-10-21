# Decentralized Land Registry

## Overview

This project implements a Decentralized Land Registry system using smart contracts on the Stacks blockchain. It allows for the registration, transfer, and management of property ownership in a transparent and secure manner.

## Features

- Property Registration: Users can register new properties with unique identifiers and details.
- Property Transfer: Owners can initiate transfers of their properties to new owners.
- Transfer Acceptance: New owners can accept property transfers to complete the ownership change.
- Property Details Retrieval: Anyone can query the details of a registered property.
- Transfer Details Retrieval: Users can check the status of ongoing property transfers.

## Smart Contract

The main smart contract (`land-registry.clar`) is written in Clarity and provides the following functions:

- `register-property`: Register a new property with a unique ID and details.
- `transfer-property`: Initiate a transfer of property ownership.
- `accept-transfer`: Accept a pending property transfer.
- `get-property-details`: Retrieve details of a registered property.
- `get-transfer-details`: Get information about a pending property transfer.

## Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet): A Clarity runtime packaged as a command line tool.
- [Node.js](https://nodejs.org/) and npm (for running additional scripts if needed)

## Setup

1. Clone the repository:
   ```
   git clone https://github.com/TheSoftNode/decentralized-land-registry.git
   cd decentralized-land-registry
   ```

2. Install Clarinet by following the instructions in the [Clarinet documentation](https://github.com/hirosystems/clarinet#installation).

3. Initialize the Clarinet project (if not already done):
   ```
   clarinet new
   ```

4. Copy the `land-registry.clar` contract into the `contracts` directory.

5. Copy the test file `land-registry_test.ts` into the `tests` directory.

## Running Tests

To run the test suite:

```
clarinet test
```

This will execute all the tests defined in `land-registry_test.ts`.

## Deployment

To deploy the contract to the Stacks blockchain:

1. Configure your Stacks account in Clarinet.
2. Run the deployment command:
   ```
   clarinet deploy
   ```

Note: Make sure you have sufficient STX tokens for deployment.

## Usage

Once deployed, you can interact with the contract using the Stacks CLI or by integrating it into a dApp. Here are some example interactions:

1. Register a property:
   ```
   stx call register-property u1 "123 Main St, Anytown USA"
   ```

2. Transfer a property:
   ```
   stx call transfer-property u1 ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM
   ```

3. Accept a property transfer:
   ```
   stx call accept-transfer u1
   ```

4. Get property details:
   ```
   stx call get-property-details u1
   ```

Replace `u1` with the appropriate property ID and the principal with the actual Stacks address when using these commands.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.
