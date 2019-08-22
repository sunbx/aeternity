#!/bin/sh

echo "%%" `cat /proc/cpuinfo | grep 'model name' | uniq`

# empty push dupa dup pop inca inc deca dec add sub mul div mod pow store sha3 sha256 blake2b lt gt eq elt egt neq and or not tuple element setelement map_empty map_lookup map_lookupd map_update map_delete map_member map_from_list is_nil cons hd tl length nil str_join int_to_str addr_to_str str_reverse append int_to_addr variant variant_test variant variant_element bits_nonea bits_none bits_alla bits_all bits_all_n bits_set bits_clear bits_sum bits_test bits_or bits_and bits_diff balance origin caller gasprice
for op in blockhash
          do
              ./_build/local/bin/aefateasm ~/aeternity/aebytecode/test/asm_code/${op}_loop.fate -o /tmp/${op}_loop.fate.bc


              for i in 1 10 100 1000 10000 100000
              do
                  TIME=`./_build/local/bin/aefate /tmp/${op}_loop.fate.bc -t  "run($i)"  | grep "Execution Time" | awk '{print $3}'`
                  printf "%10s, %8d, %16d\n"  "\"$op\"" $i $TIME
              done
              rm /tmp/${op}_loop.fate.bc

done
