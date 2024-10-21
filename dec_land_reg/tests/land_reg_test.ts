import { Clarinet, Tx, Chain, Account, types } from 'https://deno.land/x/clarinet@v0.14.0/index.ts';
import { assertEquals } from 'https://deno.land/std@0.90.0/testing/asserts.ts';

Clarinet.test({
    name: "Ensure that a property can be registered",
    async fn(chain: Chain, accounts: Map<string, Account>)
    {
        const deployer = accounts.get('deployer')!;
        const user1 = accounts.get('wallet_1')!;

        let block = chain.mineBlock([
            Tx.contractCall('land-registry', 'register-property', [types.uint(1), types.ascii("Property 1 details")], user1.address)
        ]);

        assertEquals(block.receipts.length, 1);
        assertEquals(block.height, 2);
        assertEquals(block.receipts[0].result, '(ok true)');

        // Verify the property details
        block = chain.mineBlock([
            Tx.contractCall('land-registry', 'get-property-details', [types.uint(1)], user1.address)
        ]);

        assertEquals(block.receipts.length, 1);
        assertEquals(block.height, 3);
        assertEquals(block.receipts[0].result, `(some {owner: ${user1.address}, details: "Property 1 details"})`);
    },
});

Clarinet.test({
    name: "Ensure that a property cannot be registered twice",
    async fn(chain: Chain, accounts: Map<string, Account>)
    {
        const user1 = accounts.get('wallet_1')!;

        let block = chain.mineBlock([
            Tx.contractCall('land-registry', 'register-property', [types.uint(1), types.ascii("Property 1 details")], user1.address),
            Tx.contractCall('land-registry', 'register-property', [types.uint(1), types.ascii("Duplicate property")], user1.address)
        ]);

        assertEquals(block.receipts.length, 2);
        assertEquals(block.height, 2);
        assertEquals(block.receipts[0].result, '(ok true)');
        assertEquals(block.receipts[1].result, '(err u102)'); // err-already-registered
    },
});

Clarinet.test({
    name: "Ensure that a property can be transferred",
    async fn(chain: Chain, accounts: Map<string, Account>)
    {
        const user1 = accounts.get('wallet_1')!;
        const user2 = accounts.get('wallet_2')!;

        let block = chain.mineBlock([
            Tx.contractCall('land-registry', 'register-property', [types.uint(1), types.ascii("Property 1 details")], user1.address),
            Tx.contractCall('land-registry', 'transfer-property', [types.uint(1), types.principal(user2.address)], user1.address)
        ]);

        assertEquals(block.receipts.length, 2);
        assertEquals(block.height, 2);
        assertEquals(block.receipts[1].result, '(ok true)');

        // Verify the transfer details
        block = chain.mineBlock([
            Tx.contractCall('land-registry', 'get-transfer-details', [types.uint(1)], user1.address)
        ]);

        assertEquals(block.receipts.length, 1);
        assertEquals(block.height, 3);
        assertEquals(block.receipts[0].result, `(some {from: ${user1.address}, to: ${user2.address}, status: "pending"})`);
    },
});


Clarinet.test({
    name: "Ensure that a property transfer can be accepted",
    async fn(chain: Chain, accounts: Map<string, Account>)
    {
        const user1 = accounts.get('wallet_1')!;
        const user2 = accounts.get('wallet_2')!;

        let block = chain.mineBlock([
            Tx.contractCall('land-registry', 'register-property', [types.uint(1), types.ascii("Property 1 details")], user1.address),
            Tx.contractCall('land-registry', 'transfer-property', [types.uint(1), types.principal(user2.address)], user1.address),
            Tx.contractCall('land-registry', 'accept-transfer', [types.uint(1)], user2.address)
        ]);

        assertEquals(block.receipts.length, 3);
        assertEquals(block.height, 2);
        assertEquals(block.receipts[2].result, '(ok true)');

        // Verify the new owner
        block = chain.mineBlock([
            Tx.contractCall('land-registry', 'get-property-details', [types.uint(1)], user1.address)
        ]);

        assertEquals(block.receipts.length, 1);
        assertEquals(block.height, 3);
        assertEquals(block.receipts[0].result, `(some {owner: ${user2.address}, details: "Property 1 details"})`);
    },
});

