; Data for building trees
literalSymbolCodeLength
	.ds 256
controlSymbolCodeLength
	.ds CONTROL_SYMBOLS

; Huffman trees
nBitCode_clearFrom
nBitCode_totalCount
	.ds 2*TREE_SIZE
nBitCode_literalCount
	.ds TREE_SIZE
nBitCode_controlCount
	.ds 2*TREE_SIZE
nBitCode_literalOffset
	.ds TREE_SIZE
nBitCode_controlOffset
	.ds 2*TREE_SIZE

codeToLiteralSymbol
	.ds 256
codeToControlSymbol
	.ds CONTROL_SYMBOLS

