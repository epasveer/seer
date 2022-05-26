
-data-disassemble -s $pc -e "$pc + 96" -- 0

^done,asm_insns=[
                    {address="0x000000000040093c",func-name="main()",offset="362",inst="mov    $0x400aa9,%edi"},
                    {address="0x0000000000400941",func-name="main()",offset="367",inst="call   0x400680 <puts@plt>"},
                    {address="0x0000000000400946",func-name="main()",offset="372",inst="mov    -0x30(%rbp),%rax"},
                    {address="0x000000000040094a",func-name="main()",offset="376",inst="mov    %rax,%rdi"},
                    {address="0x000000000040094d",func-name="main()",offset="379",inst="call   0x400650 <free@plt>"},
                    {address="0x0000000000400952",func-name="main()",offset="384",inst="mov    -0x38(%rbp),%rax"},
                    {address="0x0000000000400956",func-name="main()",offset="388",inst="mov    %rax,%rdi"},
                    {address="0x0000000000400959",func-name="main()",offset="391",inst="call   0x400650 <free@plt>"},
                    {address="0x000000000040095e",func-name="main()",offset="396",inst="mov    -0x40(%rbp),%rax"},
                    {address="0x0000000000400962",func-name="main()",offset="400",inst="mov    %rax,%rdi"},
                    {address="0x0000000000400965",func-name="main()",offset="403",inst="call   0x400650 <free@plt>"},
                    {address="0x000000000040096a",func-name="main()",offset="408",inst="mov    -0x60(%rbp),%rax"},
                    {address="0x000000000040096e",func-name="main()",offset="412",inst="mov    %rax,%rdi"},
                    {address="0x0000000000400971",func-name="main()",offset="415",inst="call   0x400650 <free@plt>"},
                    {address="0x0000000000400976",func-name="main()",offset="420",inst="mov    -0x68(%rbp),%rax"},
                    {address="0x000000000040097a",func-name="main()",offset="424",inst="mov    %rax,%rdi"},
                    {address="0x000000000040097d",func-name="main()",offset="427",inst="call   0x400650 <free@plt>"},
                    {address="0x0000000000400982",func-name="main()",offset="432",inst="mov    $0x0,%eax"},
                    {address="0x0000000000400987",func-name="main()",offset="437",inst="add    $0x68,%rsp"},
                    {address="0x000000000040098b",func-name="main()",offset="441",inst="pop    %rbx"},
                    {address="0x000000000040098c",func-name="main()",offset="442",inst="pop    %rbp"},
                    {address="0x000000000040098d",func-name="main()",offset="443",inst="ret    "},
                    {address="0x000000000040098e",func-name="__static_initialization_and_destruction_0(int, int)",offset="0",inst="push   %rbp"},
                    {address="0x000000000040098f",func-name="__static_initialization_and_destruction_0(int, int)",offset="1",inst="mov    %rsp,%rbp"},
                    {address="0x0000000000400992",func-name="__static_initialization_and_destruction_0(int, int)",offset="4",inst="sub    $0x10,%rsp"},
                    {address="0x0000000000400996",func-name="__static_initialization_and_destruction_0(int, int)",offset="8",inst="mov    %edi,-0x4(%rbp)"},
                    {address="0x0000000000400999",func-name="__static_initialization_and_destruction_0(int, int)",offset="11",inst="mov    %esi,-0x8(%rbp)"}
                ]





-data-disassemble -s $pc -e "$pc + 96" -- 2

^done,asm_insns=[
                    {address="0x000000000040093c",func-name="main()",offset="362",opcodes="bf a9 0a 40 00",inst="mov    $0x400aa9,%edi"},
                    {address="0x0000000000400941",func-name="main()",offset="367",opcodes="e8 3a fd ff ff",inst="call   0x400680 <puts@plt>"},
                    {address="0x0000000000400946",func-name="main()",offset="372",opcodes="48 8b 45 d0",inst="mov    -0x30(%rbp),%rax"},
                    {address="0x000000000040094a",func-name="main()",offset="376",opcodes="48 89 c7",inst="mov    %rax,%rdi"},
                    {address="0x000000000040094d",func-name="main()",offset="379",opcodes="e8 fe fc ff ff",inst="call   0x400650 <free@plt>"},
                    {address="0x0000000000400952",func-name="main()",offset="384",opcodes="48 8b 45 c8",inst="mov    -0x38(%rbp),%rax"},
                    {address="0x0000000000400956",func-name="main()",offset="388",opcodes="48 89 c7",inst="mov    %rax,%rdi"},
                    {address="0x0000000000400959",func-name="main()",offset="391",opcodes="e8 f2 fc ff ff",inst="call   0x400650 <free@plt>"},
                    {address="0x000000000040095e",func-name="main()",offset="396",opcodes="48 8b 45 c0",inst="mov    -0x40(%rbp),%rax"},
                    {address="0x0000000000400962",func-name="main()",offset="400",opcodes="48 89 c7",inst="mov    %rax,%rdi"},
                    {address="0x0000000000400965",func-name="main()",offset="403",opcodes="e8 e6 fc ff ff",inst="call   0x400650 <free@plt>"},
                    {address="0x000000000040096a",func-name="main()",offset="408",opcodes="48 8b 45 a0",inst="mov    -0x60(%rbp),%rax"},
                    {address="0x000000000040096e",func-name="main()",offset="412",opcodes="48 89 c7",inst="mov    %rax,%rdi"},
                    {address="0x0000000000400971",func-name="main()",offset="415",opcodes="e8 da fc ff ff",inst="call   0x400650 <free@plt>"},
                    {address="0x0000000000400976",func-name="main()",offset="420",opcodes="48 8b 45 98",inst="mov    -0x68(%rbp),%rax"},
                    {address="0x000000000040097a",func-name="main()",offset="424",opcodes="48 89 c7",inst="mov    %rax,%rdi"},
                    {address="0x000000000040097d",func-name="main()",offset="427",opcodes="e8 ce fc ff ff",inst="call   0x400650 <free@plt>"},
                    {address="0x0000000000400982",func-name="main()",offset="432",opcodes="b8 00 00 00 00",inst="mov    $0x0,%eax"},
                    {address="0x0000000000400987",func-name="main()",offset="437",opcodes="48 83 c4 68",inst="add    $0x68,%rsp"},
                    {address="0x000000000040098b",func-name="main()",offset="441",opcodes="5b",inst="pop    %rbx"},
                    {address="0x000000000040098c",func-name="main()",offset="442",opcodes="5d",inst="pop    %rbp"},
                    {address="0x000000000040098d",func-name="main()",offset="443",opcodes="c3",inst="ret    "},
                    {address="0x000000000040098e",func-name="__static_initialization_and_destruction_0(int, int)",offset="0",opcodes="55",inst="push   %rbp"},
                    {address="0x000000000040098f",func-name="__static_initialization_and_destruction_0(int, int)",offset="1",opcodes="48 89 e5",inst="mov    %rsp,%rbp"},
                    {address="0x0000000000400992",func-name="__static_initialization_and_destruction_0(int, int)",offset="4",opcodes="48 83 ec 10",inst="sub    $0x10,%rsp"},
                    {address="0x0000000000400996",func-name="__static_initialization_and_destruction_0(int, int)",offset="8",opcodes="89 7d fc",inst="mov    %edi,-0x4(%rbp)"},
                    {address="0x0000000000400999",func-name="__static_initialization_and_destruction_0(int, int)",offset="11",opcodes="89 75 f8",inst="mov    %esi,-0x8(%rbp)"}
                ]




-data-disassemble -s $pc -e "$pc + 96" -- 4

^done,asm_insns=[
                    src_and_asm_line={
                                        line="72",
                                        file="helloarray.cpp",
                                        fullname="/home/erniep/Development/Peak/src/seer/tests/helloarray/helloarray.cpp",
                                        line_asm_insn=[
                                            {address="0x000000000040093c",func-name="main()",offset="362",inst="mov    $0x400aa9,%edi"},
                                            {address="0x0000000000400941",func-name="main()",offset="367",inst="call   0x400680 <puts@plt>"}
                                        ]
                                    },
                    src_and_asm_line={line="73",file="helloarray.cpp",fullname="/home/erniep/Development/Peak/src/seer/tests/helloarray/helloarray.cpp",line_asm_insn=[]},
                    src_and_asm_line={line="74",file="helloarray.cpp",fullname="/home/erniep/Development/Peak/src/seer/tests/helloarray/helloarray.cpp",line_asm_insn=[{address="0x0000000000400946",func-name="main()",offset="372",inst="mov    -0x30(%rbp),%rax"},{address="0x000000000040094a",func-name="main()",offset="376",inst="mov    %rax,%rdi"},{address="0x000000000040094d",func-name="main()",offset="379",inst="call   0x400650 <free@plt>"}]},
                    src_and_asm_line={line="75",file="helloarray.cpp",fullname="/home/erniep/Development/Peak/src/seer/tests/helloarray/helloarray.cpp",line_asm_insn=[{address="0x0000000000400952",func-name="main()",offset="384",inst="mov    -0x38(%rbp),%rax"},{address="0x0000000000400956",func-name="main()",offset="388",inst="mov    %rax,%rdi"},{address="0x0000000000400959",func-name="main()",offset="391",inst="call   0x400650 <free@plt>"}]},
                    src_and_asm_line={line="76",file="helloarray.cpp",fullname="/home/erniep/Development/Peak/src/seer/tests/helloarray/helloarray.cpp",line_asm_insn=[{address="0x000000000040095e",func-name="main()",offset="396",inst="mov    -0x40(%rbp),%rax"},{address="0x0000000000400962",func-name="main()",offset="400",inst="mov    %rax,%rdi"},{address="0x0000000000400965",func-name="main()",offset="403",inst="call   0x400650 <free@plt>"}]},
                    src_and_asm_line={line="77",file="helloarray.cpp",fullname="/home/erniep/Development/Peak/src/seer/tests/helloarray/helloarray.cpp",line_asm_insn=[{address="0x000000000040096a",func-name="main()",offset="408",inst="mov    -0x60(%rbp),%rax"},{address="0x000000000040096e",func-name="main()",offset="412",inst="mov    %rax,%rdi"},{address="0x0000000000400971",func-name="main()",offset="415",inst="call   0x400650 <free@plt>"}]},
                    src_and_asm_line={line="78",file="helloarray.cpp",fullname="/home/erniep/Development/Peak/src/seer/tests/helloarray/helloarray.cpp",line_asm_insn=[{address="0x0000000000400976",func-name="main()",offset="420",inst="mov    -0x68(%rbp),%rax"},{address="0x000000000040097a",func-name="main()",offset="424",inst="mov    %rax,%rdi"},{address="0x000000000040097d",func-name="main()",offset="427",inst="call   0x400650 <free@plt>"}]},
                    src_and_asm_line={line="79",file="helloarray.cpp",fullname="/home/erniep/Development/Peak/src/seer/tests/helloarray/helloarray.cpp",line_asm_insn=[]},
                    src_and_asm_line={line="80",file="helloarray.cpp",fullname="/home/erniep/Development/Peak/src/seer/tests/helloarray/helloarray.cpp",line_asm_insn=[{address="0x0000000000400982",func-name="main()",offset="432",inst="mov    $0x0,%eax"}]},
                    src_and_asm_line={line="81",file="helloarray.cpp",fullname="/home/erniep/Development/Peak/src/seer/tests/helloarray/helloarray.cpp",line_asm_insn=[{address="0x0000000000400987",func-name="main()",offset="437",inst="add    $0x68,%rsp"},{address="0x000000000040098b",func-name="main()",offset="441",inst="pop    %rbx"},{address="0x000000000040098c",func-name="main()",offset="442",inst="pop    %rbp"},{address="0x000000000040098d",func-name="main()",offset="443",inst="ret    "},{address="0x000000000040098e",func-name="__static_initialization_and_destruction_0(int, int)",offset="0",inst="push   %rbp"},{address="0x000000000040098f",func-name="__static_initialization_and_destruction_0(int, int)",offset="1",inst="mov    %rsp,%rbp"},{address="0x0000000000400992",func-name="__static_initialization_and_destruction_0(int, int)",offset="4",inst="sub    $0x10,%rsp"},{address="0x0000000000400996",func-name="__static_initialization_and_destruction_0(int, int)",offset="8",inst="mov    %edi,-0x4(%rbp)"},{address="0x0000000000400999",func-name="__static_initialization_and_destruction_0(int, int)",offset="11",inst="mov    %esi,-0x8(%rbp)"}]}]




-data-disassemble -s $pc -e "$pc + 96" -- 5

^done,asm_insns=[
                    src_and_asm_line={
                                        line="72",
                                        file="helloarray.cpp",
                                        fullname="/home/erniep/Development/Peak/src/seer/tests/helloarray/helloarray.cpp",
                                        line_asm_insn=[
                                            {address="0x000000000040093c",func-name="main()",offset="362",opcodes="bf a9 0a 40 00",inst="mov    $0x400aa9,%edi"},
                                            {address="0x0000000000400941",func-name="main()",offset="367",opcodes="e8 3a fd ff ff",inst="call   0x400680 <puts@plt>"}
                                        ]
                                    },
                    src_and_asm_line={line="73",file="helloarray.cpp",fullname="/home/erniep/Development/Peak/src/seer/tests/helloarray/helloarray.cpp",line_asm_insn=[]},
                    src_and_asm_line={line="74",file="helloarray.cpp",fullname="/home/erniep/Development/Peak/src/seer/tests/helloarray/helloarray.cpp",line_asm_insn=[{address="0x0000000000400946",func-name="main()",offset="372",opcodes="48 8b 45 d0",inst="mov    -0x30(%rbp),%rax"},{address="0x000000000040094a",func-name="main()",offset="376",opcodes="48 89 c7",inst="mov    %rax,%rdi"},{address="0x000000000040094d",func-name="main()",offset="379",opcodes="e8 fe fc ff ff",inst="call   0x400650 <free@plt>"}]},
                    src_and_asm_line={line="75",file="helloarray.cpp",fullname="/home/erniep/Development/Peak/src/seer/tests/helloarray/helloarray.cpp",line_asm_insn=[{address="0x0000000000400952",func-name="main()",offset="384",opcodes="48 8b 45 c8",inst="mov    -0x38(%rbp),%rax"},{address="0x0000000000400956",func-name="main()",offset="388",opcodes="48 89 c7",inst="mov    %rax,%rdi"},{address="0x0000000000400959",func-name="main()",offset="391",opcodes="e8 f2 fc ff ff",inst="call   0x400650 <free@plt>"}]},
                    src_and_asm_line={line="76",file="helloarray.cpp",fullname="/home/erniep/Development/Peak/src/seer/tests/helloarray/helloarray.cpp",line_asm_insn=[{address="0x000000000040095e",func-name="main()",offset="396",opcodes="48 8b 45 c0",inst="mov    -0x40(%rbp),%rax"},{address="0x0000000000400962",func-name="main()",offset="400",opcodes="48 89 c7",inst="mov    %rax,%rdi"},{address="0x0000000000400965",func-name="main()",offset="403",opcodes="e8 e6 fc ff ff",inst="call   0x400650 <free@plt>"}]},
                    src_and_asm_line={line="77",file="helloarray.cpp",fullname="/home/erniep/Development/Peak/src/seer/tests/helloarray/helloarray.cpp",line_asm_insn=[{address="0x000000000040096a",func-name="main()",offset="408",opcodes="48 8b 45 a0",inst="mov    -0x60(%rbp),%rax"},{address="0x000000000040096e",func-name="main()",offset="412",opcodes="48 89 c7",inst="mov    %rax,%rdi"},{address="0x0000000000400971",func-name="main()",offset="415",opcodes="e8 da fc ff ff",inst="call   0x400650 <free@plt>"}]},
                    src_and_asm_line={line="78",file="helloarray.cpp",fullname="/home/erniep/Development/Peak/src/seer/tests/helloarray/helloarray.cpp",line_asm_insn=[{address="0x0000000000400976",func-name="main()",offset="420",opcodes="48 8b 45 98",inst="mov    -0x68(%rbp),%rax"},{address="0x000000000040097a",func-name="main()",offset="424",opcodes="48 89 c7",inst="mov    %rax,%rdi"},{address="0x000000000040097d",func-name="main()",offset="427",opcodes="e8 ce fc ff ff",inst="call   0x400650 <free@plt>"}]},
                    src_and_asm_line={line="79",file="helloarray.cpp",fullname="/home/erniep/Development/Peak/src/seer/tests/helloarray/helloarray.cpp",line_asm_insn=[]},
                    src_and_asm_line={line="80",file="helloarray.cpp",fullname="/home/erniep/Development/Peak/src/seer/tests/helloarray/helloarray.cpp",line_asm_insn=[{address="0x0000000000400982",func-name="main()",offset="432",opcodes="b8 00 00 00 00",inst="mov    $0x0,%eax"}]},
                    src_and_asm_line={line="81",file="helloarray.cpp",fullname="/home/erniep/Development/Peak/src/seer/tests/helloarray/helloarray.cpp",line_asm_insn=[{address="0x0000000000400987",func-name="main()",offset="437",opcodes="48 83 c4 68",inst="add    $0x68,%rsp"},{address="0x000000000040098b",func-name="main()",offset="441",opcodes="5b",inst="pop    %rbx"},{address="0x000000000040098c",func-name="main()",offset="442",opcodes="5d",inst="pop    %rbp"},{address="0x000000000040098d",func-name="main()",offset="443",opcodes="c3",inst="ret    "},{address="0x000000000040098e",func-name="__static_initialization_and_destruction_0(int, int)",offset="0",opcodes="55",inst="push   %rbp"},{address="0x000000000040098f",func-name="__static_initialization_and_destruction_0(int, int)",offset="1",opcodes="48 89 e5",inst="mov    %rsp,%rbp"},{address="0x0000000000400992",func-name="__static_initialization_and_destruction_0(int, int)",offset="4",opcodes="48 83 ec 10",inst="sub    $0x10,%rsp"},{address="0x0000000000400996",func-name="__static_initialization_and_destruction_0(int, int)",offset="8",opcodes="89 7d fc",inst="mov    %edi,-0x4(%rbp)"},{address="0x0000000000400999",func-name="__static_initialization_and_destruction_0(int, int)",offset="11",opcodes="89 75 f8",inst="mov    %esi,-0x8(%rbp)"}]}]


