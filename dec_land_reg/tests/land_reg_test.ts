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
