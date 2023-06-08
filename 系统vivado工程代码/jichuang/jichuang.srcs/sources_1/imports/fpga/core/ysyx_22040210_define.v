

//*********************GLOABAL DEFINE*********************//

`define ysyx_22040210_RstEnable         1'b1
`define ysyx_22040210_RstDisable        1'b0
`define ysyx_22040210_WriteEnable       1'b1
`define ysyx_22040210_WriteDisable      1'b0
`define ysyx_22040210_ReadEnable        1'b1
`define ysyx_22040210_ReadDisable       1'b0
`define ysyx_22040210_MemWriteEnable    1'b1
`define ysyx_22040210_MemWriteDisable   1'b0
`define ysyx_22040210_MemReadEnable     1'b1
`define ysyx_22040210_MemReadDisable    1'b0
`define ysyx_22040210_MemReadVaild      1'b1
`define ysyx_22040210_MemReadnVaild     1'b0
`define ysyx_22040210_MemWriteVaild     1'b1
`define ysyx_22040210_MemWritenVaild    1'b0
`define ysyx_22040210_InstValid         1'b1
`define ysyx_22040210_InstnVaild        1'b0
`define ysyx_22040210_True              1'b1
`define ysyx_22040210_False             1'b0
`define ysyx_22040210_ChipEnable        1'b1
`define ysyx_22040210_ChipDisable       1'b0
`define ysyx_22040210_AluOpBus          7:0
`define ysyx_22040210_StoreSelBus       7:0
`define ysyx_22040210_AluOpBusNum       8
`define ysyx_22040210_StoreSelBusNum    8
`define ysyx_22040210_AluSelBus         2:0
`define ysyx_22040210_AluSelBusNum      3
`define ysyx_22040210_StallEnable       1'b1
`define ysyx_22040210_StallDisable      1'b0
`define ysyx_22040210_FlushEnable       1'b1
`define ysyx_22040210_FlushDisable      1'b0
`define ysyx_22040210_IsJump            1'b1
`define ysyx_22040210_NoJump            1'b0
`define ysyx_22040210_RegBUS            31:0
`define ysyx_22040210_RegAddrBus        31:0
`define ysyx_22040210_RegBUSNum         6
`define ysyx_22040210_RegAddrBusNum     5
`define ysyx_22040210_StoreSelBus       7:0

`define ysyx_22040210_InstAdderBus      31:0
`define ysyx_22040210_InstBus           31:0
`define ysyx_22040210_InstLen           32
`define ysyx_22040210_InstAdderBusNum   64
`define ysyx_22040210_RegNum            32
`define ysyx_22040210_RegNumLog2        5
`define ysyx_22040210_InstMemNum        4294
`define ysyx_22040210_InstMemNumLog2    31
`define ysyx_22040210_MemBUS            31:0
`define ysyx_22040210_MemAddrBus        31:0
`define ysyx_22040210_ZeroWord          'b0
`define ysyx_22040210_RstVector         `ysyx_22040210_InstAdderBusNum'h80000000
`define ysyx_22040210_ECALL             32'h999
`define ysyx_22040210_MEPC              32'h341
`define ysyx_22040210_MTVEC             32'h305
`define ysyx_22040210_MCAUSE            32'h342
`define ysyx_22040210_MSTATUS           32'h300

`define ysyx_22040210_NOP_REG_Addr      32'b0
`define ysyx_22040210_DPICNOSTOP        1'b0
`define ysyx_22040210_DPICSTOP          1'b1
//***************************InstBuffer***************************//
`define ysyx_22040210_InstBufferNum     8
`define ysyx_22040210_InstBufferNumLog  3
`define ysyx_22040210_SingleIssue       1'b0
`define ysyx_22040210_DualIssue         1'b1
//***************************Bpu***************************//
`define ysyx_22040210_BHRLEN           13
`define ysyx_22040210_BHR_BUS          `ysyx_22040210_BHRLEN-1:0
`define ysyx_22040210_BTB_BRANCHOP     3'b000
`define ysyx_22040210_BTB_RETOP        3'b001
`define ysyx_22040210_BTB_CSROP        3'b010
`define ysyx_22040210_BTB_CALLOP       3'b011
`define ysyx_22040210_BTB_CALLANDRETOP 3'b100
`define ysyx_22040210_BTB_JALOP        3'b101
`define ysyx_22040210_BTB_BxxOP        3'b110
//***************************Cache***************************//
`define ysyx_22040210_CacheAddrBus      31:0
`define ysyx_22040210_CacheAddrBus2     31:0
`define ysyx_22040210_CacheDataBUS      31:0
`define ysyx_22040210_CacheDataBUS2     31:0
`define ysyx_22040210_CacheWayBUS       255:0
`define ysyx_22040210_CacheBankBus      127:0
`define ysyx_22040210_CacheBankBusSel   127:0
`define ysyx_22040210_PLRU_BUS          31:0
`define ysyx_22040210_Dirty_BUS         127:0
`define ysyx_22040210_CacheIndexBus     10:5
`define ysyx_22040210_CacheTagBus       31:11
`define ysyx_22040210_CacheTagVBus      53:0
`define ysyx_22040210_CacheWay0Pick     1'b0
`define ysyx_22040210_CacheWay1Pick     1'b1
`define ysyx_22040210_CacheHit          1'b1
`define ysyx_22040210_CacheNotHit       1'b0
`define ysyx_22040210_BufferHit          1'b1
`define ysyx_22040210_BufferNotHit       1'b0
`define ysyx_22040210_BUFFER_EMPTY      2'b00
`define ysyx_22040210_BUFFER_WORK       2'b01
`define ysyx_22040210_BUFFER_FULL       2'b11
`define ysyx_22040210_WriteSelBus       7:0

`define ysyx_22040210_CACHEAXI_RIDLE         3'd0
`define ysyx_22040210_CACHEAXI_WIDLE         3'd0
`define ysyx_22040210_CACHEAXI_UNDCACHEREAD  3'd1
`define ysyx_22040210_CACHEAXI_UNDCACHEWRITE 3'd1
`define ysyx_22040210_CACHEAXI_DCACHEREAD    3'd2
`define ysyx_22040210_CACHEAXI_DCACHEWRITE   3'd2
`define ysyx_22040210_CACHEAXI_UNICACHEREAD  3'd3
`define ysyx_22040210_CACHEAXI_ICACHEREAD    3'd4
`define ysyx_22040210_CACHEPIPE_DCACHE       2'b1
`define ysyx_22040210_CACHEPIPE_DUNCACHE     2'b10

`define ysyx_22040210_DCACHENOMAL                 3'd0
`define ysyx_22040210_INVALIDREADCACHE            3'd1
`define ysyx_22040210_INVALIDCACHECHECKANDWRITE   3'd2
`define ysyx_22040210_INVALIDCACHEINCR            3'd3
`define ysyx_22040210_DCACHEINVALIDFINISH         3'd4
//***********************Instruction**********************//

//*******Instruction Opcode********//
`define ysyx_22040210_INST_TYPE_I                7'b0010011
`define ysyx_22040210_INST_TYPE_ILD              7'b0000011
`define ysyx_22040210_INST_TYPE_I64              7'b0011011
`define ysyx_22040210_INST_TYPE_R                7'b0110011
`define ysyx_22040210_INST_TYPE_R64              7'b0111011
`define ysyx_22040210_INST_TYPE_B                7'b1100011
`define ysyx_22040210_INST_TYPE_S                7'b0100011
`define ysyx_22040210_INST_Fence                 7'b0001111
`define ysyx_22040210_INST_JAL                   7'b1101111
`define ysyx_22040210_INST_JALR                  7'b1100111
`define ysyx_22040210_INST_AUIPC                 7'b0010111
`define ysyx_22040210_INST_LUI                   7'b0110111

`define ysyx_22040210_INST_CSR                   7'b1110011
//*******Instruction Funct3********//
`define ysyx_22040210_INST_JALRFUN3              3'b000
`define ysyx_22040210_INST_ADDFUN3               3'b000
`define ysyx_22040210_INST_ADDI                  3'b000 
`define ysyx_22040210_INST_ADDIW                 3'b000
`define ysyx_22040210_INST_ADDWFUN3              3'b000
`define ysyx_22040210_INST_SUBFUN3               3'b000
`define ysyx_22040210_INST_SUBWFUN3              3'b000
`define ysyx_22040210_INST_ANDFUN3               3'b111
`define ysyx_22040210_INST_ORFUN3                3'b110
`define ysyx_22040210_INST_XORFUN3               3'b100
`define ysyx_22040210_INST_ANDI                  3'b111
`define ysyx_22040210_INST_ORI                   3'b110
`define ysyx_22040210_INST_XORI                  3'b100
`define ysyx_22040210_INST_SLTFUN3               3'b010
`define ysyx_22040210_INST_SLTI                  3'b010
`define ysyx_22040210_INST_SLTIU                 3'b011
`define ysyx_22040210_INST_SLTUFUN3              3'b011
`define ysyx_22040210_INST_SLLIFUN3              3'b001
`define ysyx_22040210_INST_SRAIFUN3              3'b101
`define ysyx_22040210_INST_SRLIFUN3              3'b101
`define ysyx_22040210_INST_SLLIWFUN3             3'b001
`define ysyx_22040210_INST_SRLIWFUN3             3'b101
`define ysyx_22040210_INST_SRAIWFUN3             3'b101
`define ysyx_22040210_INST_SLLFUN3               3'b001
`define ysyx_22040210_INST_SRAFUN3               3'b101
`define ysyx_22040210_INST_SRLFUN3               3'b101
`define ysyx_22040210_INST_SLLWFUN3              3'b001
`define ysyx_22040210_INST_SRLWFUN3              3'b101
`define ysyx_22040210_INST_SRAWFUN3              3'b101
`define ysyx_22040210_INST_BEQ                   3'b000
`define ysyx_22040210_INST_BNE                   3'b001
`define ysyx_22040210_INST_BLT                   3'b100
`define ysyx_22040210_INST_BGE                   3'b101
`define ysyx_22040210_INST_BLTU                  3'b110
`define ysyx_22040210_INST_BGEU                  3'b111

`define ysyx_22040210_INST_MULFUN3               3'b000
`define ysyx_22040210_INST_MULHFUN3              3'b001
`define ysyx_22040210_INST_MULHSUFUN3            3'b010
`define ysyx_22040210_INST_MULHUFUN3             3'b011
`define ysyx_22040210_INST_DIVFUN3               3'b100
`define ysyx_22040210_INST_DIVUFUN3              3'b101
`define ysyx_22040210_INST_REMFUN3               3'b110
`define ysyx_22040210_INST_REMUFUN3              3'b111
`define ysyx_22040210_INST_MULWFUN3              3'b000
`define ysyx_22040210_INST_DIVWFUN3              3'b100
`define ysyx_22040210_INST_DIVUWFUN3             3'b101
`define ysyx_22040210_INST_REMWFUN3              3'b110
`define ysyx_22040210_INST_REMUWFUN3             3'b111

`define ysyx_22040210_INST_LB                    3'b000
`define ysyx_22040210_INST_LH                    3'b001
`define ysyx_22040210_INST_LW                    3'b010
`define ysyx_22040210_INST_LD                    3'b011
`define ysyx_22040210_INST_LBU                   3'b100
`define ysyx_22040210_INST_LHU                   3'b101
`define ysyx_22040210_INST_LWU                   3'b110
`define ysyx_22040210_INST_SB                    3'b000
`define ysyx_22040210_INST_SH                    3'b001
`define ysyx_22040210_INST_SW                    3'b010
`define ysyx_22040210_INST_SD                    3'b011

`define ysyx_22040210_INST_NOP                   3'b000
`define ysyx_22040210_INST_ENVIRONMENT           3'b000
`define ysyx_22040210_INST_CSRRC                 3'b011
`define ysyx_22040210_INST_CSRRS                 3'b010
`define ysyx_22040210_INST_CSRRW                 3'b001

//*******Instruction Funct6********//
`define ysyx_22040210_INST_SLLI                  6'b000000
`define ysyx_22040210_INST_SRAI                  6'b010000
`define ysyx_22040210_INST_SRLI                  6'b000000
`define ysyx_22040210_INST_SLLIW                 6'b000000
`define ysyx_22040210_INST_SRLIW                 6'b000000
`define ysyx_22040210_INST_SRAIW                 6'b010000
//*******Instruction Funct7********//
`define ysyx_22040210_INST_ADD                   7'b0000000
`define ysyx_22040210_INST_ADDW                  7'b0000000
`define ysyx_22040210_INST_SUB                   7'b0100000
`define ysyx_22040210_INST_SUBW                  7'b0100000
`define ysyx_22040210_INST_AND                   7'b0000000
`define ysyx_22040210_INST_OR                    7'b0000000
`define ysyx_22040210_INST_XOR                   7'b0000000
`define ysyx_22040210_INST_SLT                   7'b0000000
`define ysyx_22040210_INST_SLTU                  7'b0000000
`define ysyx_22040210_INST_SLL                   7'b0000000
`define ysyx_22040210_INST_SRA                   7'b0100000
`define ysyx_22040210_INST_SRL                   7'b0000000
`define ysyx_22040210_INST_SLLW                  7'b0000000
`define ysyx_22040210_INST_SRLW                  7'b0000000
`define ysyx_22040210_INST_SRAW                  7'b0100000
`define ysyx_22040210_INST_MDU                   7'b0000001

//*******Instruction Funct12********//
`define ysyx_22040210_INST_EBREAK                12'b000000000001
`define ysyx_22040210_INST_ECALL                 12'b000000000000
`define ysyx_22040210_INST_MRET                  12'b001100000010
//***********************AluOpcode***********************//
`define ysyx_22040210_ALU_NOP_OP                 `ysyx_22040210_AluOpBusNum'd0
`define ysyx_22040210_ALU_ADD_OP                 `ysyx_22040210_AluOpBusNum'd0
`define ysyx_22040210_ALU_SUB_OP                 `ysyx_22040210_AluOpBusNum'd1
`define ysyx_22040210_ALU_AND_OP                 `ysyx_22040210_AluOpBusNum'd2
`define ysyx_22040210_ALU_OR_OP                  `ysyx_22040210_AluOpBusNum'd3
`define ysyx_22040210_ALU_XOR_OP                 `ysyx_22040210_AluOpBusNum'd4
`define ysyx_22040210_ALU_COMP_OP                `ysyx_22040210_AluOpBusNum'd5
`define ysyx_22040210_ALU_COMPU_OP               `ysyx_22040210_AluOpBusNum'd6
`define ysyx_22040210_ALU_SLL_OP                 `ysyx_22040210_AluOpBusNum'd7
`define ysyx_22040210_ALU_SRL_OP                 `ysyx_22040210_AluOpBusNum'd8
`define ysyx_22040210_ALU_SRA_OP                 `ysyx_22040210_AluOpBusNum'd9
`define ysyx_22040210_ALU_BEQ_OP                 `ysyx_22040210_AluOpBusNum'd10
`define ysyx_22040210_ALU_BNE_OP                 `ysyx_22040210_AluOpBusNum'd11
`define ysyx_22040210_ALU_BLT_OP                 `ysyx_22040210_AluOpBusNum'd12
`define ysyx_22040210_ALU_BGE_OP                 `ysyx_22040210_AluOpBusNum'd13
`define ysyx_22040210_ALU_BLTU_OP                `ysyx_22040210_AluOpBusNum'd14
`define ysyx_22040210_ALU_BGEU_OP                `ysyx_22040210_AluOpBusNum'd15
`define ysyx_22040210_ALU_JAL_OP                 `ysyx_22040210_AluOpBusNum'd16
`define ysyx_22040210_ALU_JALR_OP                `ysyx_22040210_AluOpBusNum'd17
`define ysyx_22040210_ALU_AUIPC_OP               `ysyx_22040210_AluOpBusNum'd18
`define ysyx_22040210_ALU_LUI_OP                 `ysyx_22040210_AluOpBusNum'd19
`define ysyx_22040210_ALU_LB_OP                  `ysyx_22040210_AluOpBusNum'd20
`define ysyx_22040210_ALU_LH_OP                  `ysyx_22040210_AluOpBusNum'd21
`define ysyx_22040210_ALU_LW_OP                  `ysyx_22040210_AluOpBusNum'd22
`define ysyx_22040210_ALU_LD_OP                  `ysyx_22040210_AluOpBusNum'd23
`define ysyx_22040210_ALU_LBU_OP                 `ysyx_22040210_AluOpBusNum'd24
`define ysyx_22040210_ALU_LHU_OP                 `ysyx_22040210_AluOpBusNum'd25
`define ysyx_22040210_ALU_LWU_OP                 `ysyx_22040210_AluOpBusNum'd26
`define ysyx_22040210_ALU_SB_OP                  `ysyx_22040210_AluOpBusNum'd27
`define ysyx_22040210_ALU_SH_OP                  `ysyx_22040210_AluOpBusNum'd28
`define ysyx_22040210_ALU_SW_OP                  `ysyx_22040210_AluOpBusNum'd29
`define ysyx_22040210_ALU_SD_OP                  `ysyx_22040210_AluOpBusNum'd30
`define ysyx_22040210_ALU_SLLW_OP                `ysyx_22040210_AluOpBusNum'd31
`define ysyx_22040210_ALU_SRLW_OP                `ysyx_22040210_AluOpBusNum'd32
`define ysyx_22040210_ALU_SRAW_OP                `ysyx_22040210_AluOpBusNum'd33
`define ysyx_22040210_ALU_MUL_OP                 `ysyx_22040210_AluOpBusNum'd34
`define ysyx_22040210_ALU_MULH_OP                `ysyx_22040210_AluOpBusNum'd35
`define ysyx_22040210_ALU_MULHSU_OP              `ysyx_22040210_AluOpBusNum'd36
`define ysyx_22040210_ALU_MULHU_OP               `ysyx_22040210_AluOpBusNum'd37
`define ysyx_22040210_ALU_DIV_OP                 `ysyx_22040210_AluOpBusNum'd38
`define ysyx_22040210_ALU_DIVU_OP                `ysyx_22040210_AluOpBusNum'd39
`define ysyx_22040210_ALU_DIVW_OP                `ysyx_22040210_AluOpBusNum'd40
`define ysyx_22040210_ALU_DIVUW_OP               `ysyx_22040210_AluOpBusNum'd41
`define ysyx_22040210_ALU_REM_OP                 `ysyx_22040210_AluOpBusNum'd42
`define ysyx_22040210_ALU_REMU_OP                `ysyx_22040210_AluOpBusNum'd43
`define ysyx_22040210_ALU_REMW_OP                `ysyx_22040210_AluOpBusNum'd44
`define ysyx_22040210_ALU_REMUW_OP               `ysyx_22040210_AluOpBusNum'd45
`define ysyx_22040210_ALU_ECALL_OP               `ysyx_22040210_AluOpBusNum'd46
`define ysyx_22040210_ALU_MRET_OP                `ysyx_22040210_AluOpBusNum'd47
`define ysyx_22040210_ALU_CSRRC_OP               `ysyx_22040210_AluOpBusNum'd48
`define ysyx_22040210_ALU_CSRRS_OP               `ysyx_22040210_AluOpBusNum'd49
`define ysyx_22040210_ALU_CSRRW_OP               `ysyx_22040210_AluOpBusNum'd50
`define ysyx_22040210_Fencei_OP                   `ysyx_22040210_AluOpBusNum'd51
//*************************AluSel*************************//
`define ysyx_22040210_ALU_NOP_SEL                `ysyx_22040210_AluSelBusNum'd0
`define ysyx_22040210_ALU_ARITHMETIC_SEL         `ysyx_22040210_AluSelBusNum'd0
`define ysyx_22040210_ALU_LOGIC_SEL              `ysyx_22040210_AluSelBusNum'd1
`define ysyx_22040210_ALU_64W_SEL                `ysyx_22040210_AluSelBusNum'd2
`define ysyx_22040210_ALU_LOAD_SEL               `ysyx_22040210_AluSelBusNum'd3
`define ysyx_22040210_ALU_STORE_SEL              `ysyx_22040210_AluSelBusNum'd4
`define ysyx_22040210_ALU_64LOGICW_SEL           `ysyx_22040210_AluSelBusNum'd5
`define ysyx_22040210_ALU_CSR_SEL                `ysyx_22040210_AluSelBusNum'd6

//*************************END*************************//
//AXI
`define AXRESP_OKAY             2'b00
`define AXRESP_EXOKAY           2'b01
`define AXRESP_SLVERR           2'b10
`define AXI_ARCACHE_NORMAL_NON_CACHEABLE_NON_BUFFERABLE 2'b11

`define AXLOCK_EXCLUSIVE        2'b01
`define AXLOCK_LOCKED           2'b10

`define AXBURST_FIXED           2'b00
`define AXI_BURST_TYPE_INCR     2'b01
`define AXBURST_WRAP            2'b10

`define AXSIZE_FOUR_BYTE        3'b010

`define AXCACHE_REG_BUFFER      2'b00
`define AXCACHE_REG_CACHE       2'b01
`define AXCACHE_REG_READ_ALCT   2'b10
`define AXCACHE_REG_WRITE_ALCT  2'b11

`define AXPROT_REG_NORM_OR_PRI  2'b00
`define AXPROT_REG_SEC_OR_NSEC  2'b10
`define AXPROT_REG_INST_OR_DATA 2'b01
//burst
`define AXSIZE   2:0
`define AXLEN    3:0
`define AXBURST  1:0
`define AXI_IDLE 3'b000
`define ARREADY  3'b001   //wait for arready
`define RVALID   3'b010   //wait for rvalid
`define RLAST    3'b011   //wait for rlast
`define AWREADY  3'b100   //wait for awready
`define WREADY   3'b101   //wair for wready    
`define BVALID   3'b110   //wait for bvalid
