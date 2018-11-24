pragma Warnings (Off);
pragma Ada_95;
with System;
with System.Parameters;
with System.Secondary_Stack;
package ada_main is

   gnat_argc : Integer;
   gnat_argv : System.Address;
   gnat_envp : System.Address;

   pragma Import (C, gnat_argc);
   pragma Import (C, gnat_argv);
   pragma Import (C, gnat_envp);

   gnat_exit_status : Integer;
   pragma Import (C, gnat_exit_status);

   GNAT_Version : constant String :=
                    "GNAT Version: Community 2018 (20180523-73)" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   Ada_Main_Program_Name : constant String := "_ada_swarm" & ASCII.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure adafinal;
   pragma Export (C, adafinal, "adafinal");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer;
   pragma Export (C, main, "main");

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#5ff99a27#;
   pragma Export (C, u00001, "swarmB");
   u00002 : constant Version_32 := 16#050ff2f0#;
   pragma Export (C, u00002, "system__standard_libraryB");
   u00003 : constant Version_32 := 16#0f7d71d4#;
   pragma Export (C, u00003, "system__standard_libraryS");
   u00004 : constant Version_32 := 16#f8088b52#;
   pragma Export (C, u00004, "ada__exceptionsB");
   u00005 : constant Version_32 := 16#16307b94#;
   pragma Export (C, u00005, "ada__exceptionsS");
   u00006 : constant Version_32 := 16#76789da1#;
   pragma Export (C, u00006, "adaS");
   u00007 : constant Version_32 := 16#5726abed#;
   pragma Export (C, u00007, "ada__exceptions__last_chance_handlerB");
   u00008 : constant Version_32 := 16#41e5552e#;
   pragma Export (C, u00008, "ada__exceptions__last_chance_handlerS");
   u00009 : constant Version_32 := 16#085b6ffb#;
   pragma Export (C, u00009, "systemS");
   u00010 : constant Version_32 := 16#ae860117#;
   pragma Export (C, u00010, "system__soft_linksB");
   u00011 : constant Version_32 := 16#4d58644d#;
   pragma Export (C, u00011, "system__soft_linksS");
   u00012 : constant Version_32 := 16#bd45c2cc#;
   pragma Export (C, u00012, "system__secondary_stackB");
   u00013 : constant Version_32 := 16#4dcf97e2#;
   pragma Export (C, u00013, "system__secondary_stackS");
   u00014 : constant Version_32 := 16#86dbf443#;
   pragma Export (C, u00014, "system__parametersB");
   u00015 : constant Version_32 := 16#40b73bd0#;
   pragma Export (C, u00015, "system__parametersS");
   u00016 : constant Version_32 := 16#ced09590#;
   pragma Export (C, u00016, "system__storage_elementsB");
   u00017 : constant Version_32 := 16#259825ff#;
   pragma Export (C, u00017, "system__storage_elementsS");
   u00018 : constant Version_32 := 16#75bf515c#;
   pragma Export (C, u00018, "system__soft_links__initializeB");
   u00019 : constant Version_32 := 16#5697fc2b#;
   pragma Export (C, u00019, "system__soft_links__initializeS");
   u00020 : constant Version_32 := 16#41837d1e#;
   pragma Export (C, u00020, "system__stack_checkingB");
   u00021 : constant Version_32 := 16#86e40413#;
   pragma Export (C, u00021, "system__stack_checkingS");
   u00022 : constant Version_32 := 16#34742901#;
   pragma Export (C, u00022, "system__exception_tableB");
   u00023 : constant Version_32 := 16#55f506b9#;
   pragma Export (C, u00023, "system__exception_tableS");
   u00024 : constant Version_32 := 16#ce4af020#;
   pragma Export (C, u00024, "system__exceptionsB");
   u00025 : constant Version_32 := 16#6038020d#;
   pragma Export (C, u00025, "system__exceptionsS");
   u00026 : constant Version_32 := 16#80916427#;
   pragma Export (C, u00026, "system__exceptions__machineB");
   u00027 : constant Version_32 := 16#3bad9081#;
   pragma Export (C, u00027, "system__exceptions__machineS");
   u00028 : constant Version_32 := 16#aa0563fc#;
   pragma Export (C, u00028, "system__exceptions_debugB");
   u00029 : constant Version_32 := 16#76d1963f#;
   pragma Export (C, u00029, "system__exceptions_debugS");
   u00030 : constant Version_32 := 16#6c2f8802#;
   pragma Export (C, u00030, "system__img_intB");
   u00031 : constant Version_32 := 16#0a808f39#;
   pragma Export (C, u00031, "system__img_intS");
   u00032 : constant Version_32 := 16#39df8c17#;
   pragma Export (C, u00032, "system__tracebackB");
   u00033 : constant Version_32 := 16#5679b13f#;
   pragma Export (C, u00033, "system__tracebackS");
   u00034 : constant Version_32 := 16#9ed49525#;
   pragma Export (C, u00034, "system__traceback_entriesB");
   u00035 : constant Version_32 := 16#0800998b#;
   pragma Export (C, u00035, "system__traceback_entriesS");
   u00036 : constant Version_32 := 16#bb296fbb#;
   pragma Export (C, u00036, "system__traceback__symbolicB");
   u00037 : constant Version_32 := 16#c84061d1#;
   pragma Export (C, u00037, "system__traceback__symbolicS");
   u00038 : constant Version_32 := 16#701f9d88#;
   pragma Export (C, u00038, "ada__exceptions__tracebackB");
   u00039 : constant Version_32 := 16#20245e75#;
   pragma Export (C, u00039, "ada__exceptions__tracebackS");
   u00040 : constant Version_32 := 16#a0d3d22b#;
   pragma Export (C, u00040, "system__address_imageB");
   u00041 : constant Version_32 := 16#a9b7f2c1#;
   pragma Export (C, u00041, "system__address_imageS");
   u00042 : constant Version_32 := 16#8c33a517#;
   pragma Export (C, u00042, "system__wch_conB");
   u00043 : constant Version_32 := 16#13264d29#;
   pragma Export (C, u00043, "system__wch_conS");
   u00044 : constant Version_32 := 16#9721e840#;
   pragma Export (C, u00044, "system__wch_stwB");
   u00045 : constant Version_32 := 16#3e376128#;
   pragma Export (C, u00045, "system__wch_stwS");
   u00046 : constant Version_32 := 16#a831679c#;
   pragma Export (C, u00046, "system__wch_cnvB");
   u00047 : constant Version_32 := 16#1c91f7da#;
   pragma Export (C, u00047, "system__wch_cnvS");
   u00048 : constant Version_32 := 16#5ab55268#;
   pragma Export (C, u00048, "interfacesS");
   u00049 : constant Version_32 := 16#ece6fdb6#;
   pragma Export (C, u00049, "system__wch_jisB");
   u00050 : constant Version_32 := 16#9ce1eefb#;
   pragma Export (C, u00050, "system__wch_jisS");
   u00051 : constant Version_32 := 16#64d7e8d2#;
   pragma Export (C, u00051, "callback_proceduresB");
   u00052 : constant Version_32 := 16#0bb57c0d#;
   pragma Export (C, u00052, "callback_proceduresS");
   u00053 : constant Version_32 := 16#adc843f0#;
   pragma Export (C, u00053, "ada__real_timeB");
   u00054 : constant Version_32 := 16#69ea8064#;
   pragma Export (C, u00054, "ada__real_timeS");
   u00055 : constant Version_32 := 16#c8eda0b7#;
   pragma Export (C, u00055, "system__taskingB");
   u00056 : constant Version_32 := 16#904e70c9#;
   pragma Export (C, u00056, "system__taskingS");
   u00057 : constant Version_32 := 16#fde20231#;
   pragma Export (C, u00057, "system__task_primitivesS");
   u00058 : constant Version_32 := 16#4a005d03#;
   pragma Export (C, u00058, "system__os_interfaceB");
   u00059 : constant Version_32 := 16#55598295#;
   pragma Export (C, u00059, "system__os_interfaceS");
   u00060 : constant Version_32 := 16#769e25e6#;
   pragma Export (C, u00060, "interfaces__cB");
   u00061 : constant Version_32 := 16#467817d8#;
   pragma Export (C, u00061, "interfaces__cS");
   u00062 : constant Version_32 := 16#1b8990a4#;
   pragma Export (C, u00062, "interfaces__c__extensionsS");
   u00063 : constant Version_32 := 16#54ea2fd2#;
   pragma Export (C, u00063, "system__os_constantsS");
   u00064 : constant Version_32 := 16#9690f83e#;
   pragma Export (C, u00064, "system__task_primitives__operationsB");
   u00065 : constant Version_32 := 16#bd0bc49c#;
   pragma Export (C, u00065, "system__task_primitives__operationsS");
   u00066 : constant Version_32 := 16#89b55e64#;
   pragma Export (C, u00066, "system__interrupt_managementB");
   u00067 : constant Version_32 := 16#1a73cd21#;
   pragma Export (C, u00067, "system__interrupt_managementS");
   u00068 : constant Version_32 := 16#f65595cf#;
   pragma Export (C, u00068, "system__multiprocessorsB");
   u00069 : constant Version_32 := 16#30f7f088#;
   pragma Export (C, u00069, "system__multiprocessorsS");
   u00070 : constant Version_32 := 16#2b2125d3#;
   pragma Export (C, u00070, "system__os_primitivesB");
   u00071 : constant Version_32 := 16#0fa60a0d#;
   pragma Export (C, u00071, "system__os_primitivesS");
   u00072 : constant Version_32 := 16#e0fce7f8#;
   pragma Export (C, u00072, "system__task_infoB");
   u00073 : constant Version_32 := 16#8841d2fa#;
   pragma Export (C, u00073, "system__task_infoS");
   u00074 : constant Version_32 := 16#1036f432#;
   pragma Export (C, u00074, "system__tasking__debugB");
   u00075 : constant Version_32 := 16#de1ac8b1#;
   pragma Export (C, u00075, "system__tasking__debugS");
   u00076 : constant Version_32 := 16#fd83e873#;
   pragma Export (C, u00076, "system__concat_2B");
   u00077 : constant Version_32 := 16#0afbb82b#;
   pragma Export (C, u00077, "system__concat_2S");
   u00078 : constant Version_32 := 16#2b70b149#;
   pragma Export (C, u00078, "system__concat_3B");
   u00079 : constant Version_32 := 16#032b335e#;
   pragma Export (C, u00079, "system__concat_3S");
   u00080 : constant Version_32 := 16#4e0ce0a1#;
   pragma Export (C, u00080, "system__crtlS");
   u00081 : constant Version_32 := 16#273384e4#;
   pragma Export (C, u00081, "system__img_enum_newB");
   u00082 : constant Version_32 := 16#6917693b#;
   pragma Export (C, u00082, "system__img_enum_newS");
   u00083 : constant Version_32 := 16#9dca6636#;
   pragma Export (C, u00083, "system__img_lliB");
   u00084 : constant Version_32 := 16#19143a2a#;
   pragma Export (C, u00084, "system__img_lliS");
   u00085 : constant Version_32 := 16#3cdd1378#;
   pragma Export (C, u00085, "system__unsigned_typesS");
   u00086 : constant Version_32 := 16#6ec3c867#;
   pragma Export (C, u00086, "system__stack_usageB");
   u00087 : constant Version_32 := 16#3a3ac346#;
   pragma Export (C, u00087, "system__stack_usageS");
   u00088 : constant Version_32 := 16#d7aac20c#;
   pragma Export (C, u00088, "system__ioB");
   u00089 : constant Version_32 := 16#961998b4#;
   pragma Export (C, u00089, "system__ioS");
   u00090 : constant Version_32 := 16#d398a95f#;
   pragma Export (C, u00090, "ada__tagsB");
   u00091 : constant Version_32 := 16#12a0afb8#;
   pragma Export (C, u00091, "ada__tagsS");
   u00092 : constant Version_32 := 16#796f31f1#;
   pragma Export (C, u00092, "system__htableB");
   u00093 : constant Version_32 := 16#8c99dc11#;
   pragma Export (C, u00093, "system__htableS");
   u00094 : constant Version_32 := 16#089f5cd0#;
   pragma Export (C, u00094, "system__string_hashB");
   u00095 : constant Version_32 := 16#2ec7b76f#;
   pragma Export (C, u00095, "system__string_hashS");
   u00096 : constant Version_32 := 16#afdbf393#;
   pragma Export (C, u00096, "system__val_lluB");
   u00097 : constant Version_32 := 16#462f440a#;
   pragma Export (C, u00097, "system__val_lluS");
   u00098 : constant Version_32 := 16#269742a9#;
   pragma Export (C, u00098, "system__val_utilB");
   u00099 : constant Version_32 := 16#a4fbd905#;
   pragma Export (C, u00099, "system__val_utilS");
   u00100 : constant Version_32 := 16#ec4d5631#;
   pragma Export (C, u00100, "system__case_utilB");
   u00101 : constant Version_32 := 16#378ed9af#;
   pragma Export (C, u00101, "system__case_utilS");
   u00102 : constant Version_32 := 16#9e3065b2#;
   pragma Export (C, u00102, "exceptionsB");
   u00103 : constant Version_32 := 16#bae1bad2#;
   pragma Export (C, u00103, "exceptionsS");
   u00104 : constant Version_32 := 16#920f3e36#;
   pragma Export (C, u00104, "ada__task_identificationB");
   u00105 : constant Version_32 := 16#a04f2339#;
   pragma Export (C, u00105, "ada__task_identificationS");
   u00106 : constant Version_32 := 16#1d5082e5#;
   pragma Export (C, u00106, "system__tasking__utilitiesB");
   u00107 : constant Version_32 := 16#ab3d060e#;
   pragma Export (C, u00107, "system__tasking__utilitiesS");
   u00108 : constant Version_32 := 16#0a98af1a#;
   pragma Export (C, u00108, "system__tasking__initializationB");
   u00109 : constant Version_32 := 16#f7885a93#;
   pragma Export (C, u00109, "system__tasking__initializationS");
   u00110 : constant Version_32 := 16#9f1b5cb1#;
   pragma Export (C, u00110, "system__soft_links__taskingB");
   u00111 : constant Version_32 := 16#e939497e#;
   pragma Export (C, u00111, "system__soft_links__taskingS");
   u00112 : constant Version_32 := 16#17d21067#;
   pragma Export (C, u00112, "ada__exceptions__is_null_occurrenceB");
   u00113 : constant Version_32 := 16#e1d7566f#;
   pragma Export (C, u00113, "ada__exceptions__is_null_occurrenceS");
   u00114 : constant Version_32 := 16#cde2b616#;
   pragma Export (C, u00114, "system__tasking__task_attributesB");
   u00115 : constant Version_32 := 16#4c40320c#;
   pragma Export (C, u00115, "system__tasking__task_attributesS");
   u00116 : constant Version_32 := 16#81b9d4a8#;
   pragma Export (C, u00116, "system__tasking__queuingB");
   u00117 : constant Version_32 := 16#c9e0262c#;
   pragma Export (C, u00117, "system__tasking__queuingS");
   u00118 : constant Version_32 := 16#9fcf5d7f#;
   pragma Export (C, u00118, "system__tasking__protected_objectsB");
   u00119 : constant Version_32 := 16#b15a1586#;
   pragma Export (C, u00119, "system__tasking__protected_objectsS");
   u00120 : constant Version_32 := 16#3d3c265e#;
   pragma Export (C, u00120, "system__tasking__protected_objects__entriesB");
   u00121 : constant Version_32 := 16#7daf93e7#;
   pragma Export (C, u00121, "system__tasking__protected_objects__entriesS");
   u00122 : constant Version_32 := 16#100eaf58#;
   pragma Export (C, u00122, "system__restrictionsB");
   u00123 : constant Version_32 := 16#4329b6aa#;
   pragma Export (C, u00123, "system__restrictionsS");
   u00124 : constant Version_32 := 16#86c56e5a#;
   pragma Export (C, u00124, "ada__finalizationS");
   u00125 : constant Version_32 := 16#10558b11#;
   pragma Export (C, u00125, "ada__streamsB");
   u00126 : constant Version_32 := 16#67e31212#;
   pragma Export (C, u00126, "ada__streamsS");
   u00127 : constant Version_32 := 16#92d882c5#;
   pragma Export (C, u00127, "ada__io_exceptionsS");
   u00128 : constant Version_32 := 16#95817ed8#;
   pragma Export (C, u00128, "system__finalization_rootB");
   u00129 : constant Version_32 := 16#47a91c6b#;
   pragma Export (C, u00129, "system__finalization_rootS");
   u00130 : constant Version_32 := 16#927a893f#;
   pragma Export (C, u00130, "ada__text_ioB");
   u00131 : constant Version_32 := 16#1ffab6e1#;
   pragma Export (C, u00131, "ada__text_ioS");
   u00132 : constant Version_32 := 16#73d2d764#;
   pragma Export (C, u00132, "interfaces__c_streamsB");
   u00133 : constant Version_32 := 16#b1330297#;
   pragma Export (C, u00133, "interfaces__c_streamsS");
   u00134 : constant Version_32 := 16#ec083f01#;
   pragma Export (C, u00134, "system__file_ioB");
   u00135 : constant Version_32 := 16#af2a8e9e#;
   pragma Export (C, u00135, "system__file_ioS");
   u00136 : constant Version_32 := 16#0f8892f9#;
   pragma Export (C, u00136, "system__os_libB");
   u00137 : constant Version_32 := 16#d8e681fb#;
   pragma Export (C, u00137, "system__os_libS");
   u00138 : constant Version_32 := 16#2a8e89ad#;
   pragma Export (C, u00138, "system__stringsB");
   u00139 : constant Version_32 := 16#684d436e#;
   pragma Export (C, u00139, "system__stringsS");
   u00140 : constant Version_32 := 16#f5c4f553#;
   pragma Export (C, u00140, "system__file_control_blockS");
   u00141 : constant Version_32 := 16#20095f7e#;
   pragma Export (C, u00141, "generic_sliding_statisticsB");
   u00142 : constant Version_32 := 16#bb10ab49#;
   pragma Export (C, u00142, "generic_sliding_statisticsS");
   u00143 : constant Version_32 := 16#98a387f5#;
   pragma Export (C, u00143, "real_typeS");
   u00144 : constant Version_32 := 16#cd2959fb#;
   pragma Export (C, u00144, "ada__numericsS");
   u00145 : constant Version_32 := 16#e5114ee9#;
   pragma Export (C, u00145, "ada__numerics__auxB");
   u00146 : constant Version_32 := 16#9f6e24ed#;
   pragma Export (C, u00146, "ada__numerics__auxS");
   u00147 : constant Version_32 := 16#0cccd408#;
   pragma Export (C, u00147, "system__fat_llfS");
   u00148 : constant Version_32 := 16#6533c8fa#;
   pragma Export (C, u00148, "system__machine_codeS");
   u00149 : constant Version_32 := 16#52f1910f#;
   pragma Export (C, u00149, "system__assertionsB");
   u00150 : constant Version_32 := 16#c5d6436f#;
   pragma Export (C, u00150, "system__assertionsS");
   u00151 : constant Version_32 := 16#b2a569d2#;
   pragma Export (C, u00151, "system__exn_llfB");
   u00152 : constant Version_32 := 16#b425d427#;
   pragma Export (C, u00152, "system__exn_llfS");
   u00153 : constant Version_32 := 16#761c7ae2#;
   pragma Export (C, u00153, "system__fat_lfltS");
   u00154 : constant Version_32 := 16#f86a0784#;
   pragma Export (C, u00154, "graphics_configurationS");
   u00155 : constant Version_32 := 16#a1e0f341#;
   pragma Export (C, u00155, "glB");
   u00156 : constant Version_32 := 16#3e6953e9#;
   pragma Export (C, u00156, "glS");
   u00157 : constant Version_32 := 16#7be628b8#;
   pragma Export (C, u00157, "gl__extendedS");
   u00158 : constant Version_32 := 16#27986d94#;
   pragma Export (C, u00158, "interfaces__c__stringsB");
   u00159 : constant Version_32 := 16#603c1c44#;
   pragma Export (C, u00159, "interfaces__c__stringsS");
   u00160 : constant Version_32 := 16#ef512720#;
   pragma Export (C, u00160, "globe_3dB");
   u00161 : constant Version_32 := 16#68f549e6#;
   pragma Export (C, u00161, "globe_3dS");
   u00162 : constant Version_32 := 16#5b4659fa#;
   pragma Export (C, u00162, "ada__charactersS");
   u00163 : constant Version_32 := 16#8f637df8#;
   pragma Export (C, u00163, "ada__characters__handlingB");
   u00164 : constant Version_32 := 16#3b3f6154#;
   pragma Export (C, u00164, "ada__characters__handlingS");
   u00165 : constant Version_32 := 16#4b7bb96a#;
   pragma Export (C, u00165, "ada__characters__latin_1S");
   u00166 : constant Version_32 := 16#e6d4fa36#;
   pragma Export (C, u00166, "ada__stringsS");
   u00167 : constant Version_32 := 16#96df1a3f#;
   pragma Export (C, u00167, "ada__strings__mapsB");
   u00168 : constant Version_32 := 16#1e526bec#;
   pragma Export (C, u00168, "ada__strings__mapsS");
   u00169 : constant Version_32 := 16#98e13b0e#;
   pragma Export (C, u00169, "system__bit_opsB");
   u00170 : constant Version_32 := 16#0765e3a3#;
   pragma Export (C, u00170, "system__bit_opsS");
   u00171 : constant Version_32 := 16#92f05f13#;
   pragma Export (C, u00171, "ada__strings__maps__constantsS");
   u00172 : constant Version_32 := 16#179d7d28#;
   pragma Export (C, u00172, "ada__containersS");
   u00173 : constant Version_32 := 16#adb6d201#;
   pragma Export (C, u00173, "ada__strings__fixedB");
   u00174 : constant Version_32 := 16#a86b22b3#;
   pragma Export (C, u00174, "ada__strings__fixedS");
   u00175 : constant Version_32 := 16#2eb48a6d#;
   pragma Export (C, u00175, "ada__strings__searchB");
   u00176 : constant Version_32 := 16#c1ab8667#;
   pragma Export (C, u00176, "ada__strings__searchS");
   u00177 : constant Version_32 := 16#9c2abfb0#;
   pragma Export (C, u00177, "gl__errorsB");
   u00178 : constant Version_32 := 16#51391c66#;
   pragma Export (C, u00178, "gl__errorsS");
   u00179 : constant Version_32 := 16#30237187#;
   pragma Export (C, u00179, "gluB");
   u00180 : constant Version_32 := 16#f8141e55#;
   pragma Export (C, u00180, "gluS");
   u00181 : constant Version_32 := 16#932a4690#;
   pragma Export (C, u00181, "system__concat_4B");
   u00182 : constant Version_32 := 16#763f44db#;
   pragma Export (C, u00182, "system__concat_4S");
   u00183 : constant Version_32 := 16#049d363d#;
   pragma Export (C, u00183, "gl__skinsB");
   u00184 : constant Version_32 := 16#2584189e#;
   pragma Export (C, u00184, "gl__skinsS");
   u00185 : constant Version_32 := 16#502e73ef#;
   pragma Export (C, u00185, "system__fat_fltS");
   u00186 : constant Version_32 := 16#5a895de2#;
   pragma Export (C, u00186, "system__pool_globalB");
   u00187 : constant Version_32 := 16#7141203e#;
   pragma Export (C, u00187, "system__pool_globalS");
   u00188 : constant Version_32 := 16#2323a8af#;
   pragma Export (C, u00188, "system__memoryB");
   u00189 : constant Version_32 := 16#512609cf#;
   pragma Export (C, u00189, "system__memoryS");
   u00190 : constant Version_32 := 16#6d4d969a#;
   pragma Export (C, u00190, "system__storage_poolsB");
   u00191 : constant Version_32 := 16#2bb6f156#;
   pragma Export (C, u00191, "system__storage_poolsS");
   u00192 : constant Version_32 := 16#2e260032#;
   pragma Export (C, u00192, "system__storage_pools__subpoolsB");
   u00193 : constant Version_32 := 16#cc5a1856#;
   pragma Export (C, u00193, "system__storage_pools__subpoolsS");
   u00194 : constant Version_32 := 16#d96e3c40#;
   pragma Export (C, u00194, "system__finalization_mastersB");
   u00195 : constant Version_32 := 16#53a75631#;
   pragma Export (C, u00195, "system__finalization_mastersS");
   u00196 : constant Version_32 := 16#7268f812#;
   pragma Export (C, u00196, "system__img_boolB");
   u00197 : constant Version_32 := 16#fd821e10#;
   pragma Export (C, u00197, "system__img_boolS");
   u00198 : constant Version_32 := 16#84042202#;
   pragma Export (C, u00198, "system__storage_pools__subpools__finalizationB");
   u00199 : constant Version_32 := 16#fe2f4b3a#;
   pragma Export (C, u00199, "system__storage_pools__subpools__finalizationS");
   u00200 : constant Version_32 := 16#837026c9#;
   pragma Export (C, u00200, "gl__bufferB");
   u00201 : constant Version_32 := 16#fcf76790#;
   pragma Export (C, u00201, "gl__bufferS");
   u00202 : constant Version_32 := 16#039168f8#;
   pragma Export (C, u00202, "system__stream_attributesB");
   u00203 : constant Version_32 := 16#8bc30a4e#;
   pragma Export (C, u00203, "system__stream_attributesS");
   u00204 : constant Version_32 := 16#e6b92a39#;
   pragma Export (C, u00204, "gl__buffer__texture_coordsB");
   u00205 : constant Version_32 := 16#ee91d95f#;
   pragma Export (C, u00205, "gl__buffer__texture_coordsS");
   u00206 : constant Version_32 := 16#9004b2de#;
   pragma Export (C, u00206, "gl__buffer__generalB");
   u00207 : constant Version_32 := 16#b07eab0a#;
   pragma Export (C, u00207, "gl__buffer__generalS");
   u00208 : constant Version_32 := 16#089a501d#;
   pragma Export (C, u00208, "gl__geometryB");
   u00209 : constant Version_32 := 16#1e8d36a3#;
   pragma Export (C, u00209, "gl__geometryS");
   u00210 : constant Version_32 := 16#457fb2da#;
   pragma Export (C, u00210, "ada__strings__unboundedB");
   u00211 : constant Version_32 := 16#f39c7224#;
   pragma Export (C, u00211, "ada__strings__unboundedS");
   u00212 : constant Version_32 := 16#acee74ad#;
   pragma Export (C, u00212, "system__compare_array_unsigned_8B");
   u00213 : constant Version_32 := 16#a1581e76#;
   pragma Export (C, u00213, "system__compare_array_unsigned_8S");
   u00214 : constant Version_32 := 16#a8025f3c#;
   pragma Export (C, u00214, "system__address_operationsB");
   u00215 : constant Version_32 := 16#1b57d1c8#;
   pragma Export (C, u00215, "system__address_operationsS");
   u00216 : constant Version_32 := 16#020a3f4d#;
   pragma Export (C, u00216, "system__atomic_countersB");
   u00217 : constant Version_32 := 16#bc074276#;
   pragma Export (C, u00217, "system__atomic_countersS");
   u00218 : constant Version_32 := 16#59ec45db#;
   pragma Export (C, u00218, "gl__mathB");
   u00219 : constant Version_32 := 16#a8dd0043#;
   pragma Export (C, u00219, "gl__mathS");
   u00220 : constant Version_32 := 16#d5f9759f#;
   pragma Export (C, u00220, "ada__text_io__float_auxB");
   u00221 : constant Version_32 := 16#48248c7b#;
   pragma Export (C, u00221, "ada__text_io__float_auxS");
   u00222 : constant Version_32 := 16#181dc502#;
   pragma Export (C, u00222, "ada__text_io__generic_auxB");
   u00223 : constant Version_32 := 16#16b3615d#;
   pragma Export (C, u00223, "ada__text_io__generic_auxS");
   u00224 : constant Version_32 := 16#8aa4f090#;
   pragma Export (C, u00224, "system__img_realB");
   u00225 : constant Version_32 := 16#cff33e19#;
   pragma Export (C, u00225, "system__img_realS");
   u00226 : constant Version_32 := 16#1b28662b#;
   pragma Export (C, u00226, "system__float_controlB");
   u00227 : constant Version_32 := 16#e8a72cc7#;
   pragma Export (C, u00227, "system__float_controlS");
   u00228 : constant Version_32 := 16#3e932977#;
   pragma Export (C, u00228, "system__img_lluB");
   u00229 : constant Version_32 := 16#751413bb#;
   pragma Export (C, u00229, "system__img_lluS");
   u00230 : constant Version_32 := 16#ec78c2bf#;
   pragma Export (C, u00230, "system__img_unsB");
   u00231 : constant Version_32 := 16#a3292f8f#;
   pragma Export (C, u00231, "system__img_unsS");
   u00232 : constant Version_32 := 16#582b098c#;
   pragma Export (C, u00232, "system__powten_tableS");
   u00233 : constant Version_32 := 16#c2ca0511#;
   pragma Export (C, u00233, "system__val_realB");
   u00234 : constant Version_32 := 16#f67218ea#;
   pragma Export (C, u00234, "system__val_realS");
   u00235 : constant Version_32 := 16#608e2cd1#;
   pragma Export (C, u00235, "system__concat_5B");
   u00236 : constant Version_32 := 16#8f052cd5#;
   pragma Export (C, u00236, "system__concat_5S");
   u00237 : constant Version_32 := 16#fafb0eca#;
   pragma Export (C, u00237, "gl__texturesB");
   u00238 : constant Version_32 := 16#91ca1c90#;
   pragma Export (C, u00238, "gl__texturesS");
   u00239 : constant Version_32 := 16#5c76d753#;
   pragma Export (C, u00239, "gl__ioB");
   u00240 : constant Version_32 := 16#daf3b9dd#;
   pragma Export (C, u00240, "gl__ioS");
   u00241 : constant Version_32 := 16#86ecf8ab#;
   pragma Export (C, u00241, "system__strings__stream_opsB");
   u00242 : constant Version_32 := 16#ec029138#;
   pragma Export (C, u00242, "system__strings__stream_opsS");
   u00243 : constant Version_32 := 16#db0aa7dc#;
   pragma Export (C, u00243, "ada__streams__stream_ioB");
   u00244 : constant Version_32 := 16#55e6e4b0#;
   pragma Export (C, u00244, "ada__streams__stream_ioS");
   u00245 : constant Version_32 := 16#5de653db#;
   pragma Export (C, u00245, "system__communicationB");
   u00246 : constant Version_32 := 16#113b3a29#;
   pragma Export (C, u00246, "system__communicationS");
   u00247 : constant Version_32 := 16#7f8879fc#;
   pragma Export (C, u00247, "gl__materialsB");
   u00248 : constant Version_32 := 16#65f2b4a3#;
   pragma Export (C, u00248, "gl__materialsS");
   u00249 : constant Version_32 := 16#33fd7c82#;
   pragma Export (C, u00249, "globe_3d__mathB");
   u00250 : constant Version_32 := 16#e3e98860#;
   pragma Export (C, u00250, "globe_3d__mathS");
   u00251 : constant Version_32 := 16#40c8be3a#;
   pragma Export (C, u00251, "globe_3d__optionsB");
   u00252 : constant Version_32 := 16#dc499730#;
   pragma Export (C, u00252, "globe_3d__optionsS");
   u00253 : constant Version_32 := 16#bd43c68c#;
   pragma Export (C, u00253, "globe_3d__portalsB");
   u00254 : constant Version_32 := 16#345614ab#;
   pragma Export (C, u00254, "globe_3d__portalsS");
   u00255 : constant Version_32 := 16#c861171e#;
   pragma Export (C, u00255, "globe_3d__texturesB");
   u00256 : constant Version_32 := 16#2a1c4756#;
   pragma Export (C, u00256, "globe_3d__texturesS");
   u00257 : constant Version_32 := 16#c164a034#;
   pragma Export (C, u00257, "ada__containers__hash_tablesS");
   u00258 : constant Version_32 := 16#bcec81df#;
   pragma Export (C, u00258, "ada__containers__helpersB");
   u00259 : constant Version_32 := 16#4adfc5eb#;
   pragma Export (C, u00259, "ada__containers__helpersS");
   u00260 : constant Version_32 := 16#c24eaf4d#;
   pragma Export (C, u00260, "ada__containers__prime_numbersB");
   u00261 : constant Version_32 := 16#6d3af8ed#;
   pragma Export (C, u00261, "ada__containers__prime_numbersS");
   u00262 : constant Version_32 := 16#217daf40#;
   pragma Export (C, u00262, "ada__strings__unbounded__hashB");
   u00263 : constant Version_32 := 16#f0232cad#;
   pragma Export (C, u00263, "ada__strings__unbounded__hashS");
   u00264 : constant Version_32 := 16#fa5081d0#;
   pragma Export (C, u00264, "unzipB");
   u00265 : constant Version_32 := 16#592613ef#;
   pragma Export (C, u00265, "unzipS");
   u00266 : constant Version_32 := 16#8bbf26e6#;
   pragma Export (C, u00266, "unzip__decompressB");
   u00267 : constant Version_32 := 16#ff9edbf7#;
   pragma Export (C, u00267, "unzip__decompressS");
   u00268 : constant Version_32 := 16#bf116e3f#;
   pragma Export (C, u00268, "bzip2B");
   u00269 : constant Version_32 := 16#951df2a7#;
   pragma Export (C, u00269, "bzip2S");
   u00270 : constant Version_32 := 16#297a02d7#;
   pragma Export (C, u00270, "unzip__decompress__huffmanB");
   u00271 : constant Version_32 := 16#3045b9d5#;
   pragma Export (C, u00271, "unzip__decompress__huffmanS");
   u00272 : constant Version_32 := 16#21f15937#;
   pragma Export (C, u00272, "zipB");
   u00273 : constant Version_32 := 16#a4865b30#;
   pragma Export (C, u00273, "zipS");
   u00274 : constant Version_32 := 16#ebb133b2#;
   pragma Export (C, u00274, "zip__headersB");
   u00275 : constant Version_32 := 16#17ef6cb6#;
   pragma Export (C, u00275, "zip__headersS");
   u00276 : constant Version_32 := 16#97991e4b#;
   pragma Export (C, u00276, "zip_streamsB");
   u00277 : constant Version_32 := 16#534a2c31#;
   pragma Export (C, u00277, "zip_streamsS");
   u00278 : constant Version_32 := 16#b8719323#;
   pragma Export (C, u00278, "ada__calendarB");
   u00279 : constant Version_32 := 16#41508869#;
   pragma Export (C, u00279, "ada__calendarS");
   u00280 : constant Version_32 := 16#397e9c9f#;
   pragma Export (C, u00280, "zip__crcB");
   u00281 : constant Version_32 := 16#06c4d47b#;
   pragma Export (C, u00281, "zip__crcS");
   u00282 : constant Version_32 := 16#8ec48dfb#;
   pragma Export (C, u00282, "unzip__streamsB");
   u00283 : constant Version_32 := 16#deb9bdf2#;
   pragma Export (C, u00283, "unzip__streamsS");
   u00284 : constant Version_32 := 16#46899fd1#;
   pragma Export (C, u00284, "system__concat_7B");
   u00285 : constant Version_32 := 16#f49c34e4#;
   pragma Export (C, u00285, "system__concat_7S");
   u00286 : constant Version_32 := 16#a83b7c85#;
   pragma Export (C, u00286, "system__concat_6B");
   u00287 : constant Version_32 := 16#da9c4249#;
   pragma Export (C, u00287, "system__concat_6S");
   u00288 : constant Version_32 := 16#46b1f5ea#;
   pragma Export (C, u00288, "system__concat_8B");
   u00289 : constant Version_32 := 16#eb5c222c#;
   pragma Export (C, u00289, "system__concat_8S");
   u00290 : constant Version_32 := 16#de0a8573#;
   pragma Export (C, u00290, "gl__frustumsB");
   u00291 : constant Version_32 := 16#2c4c19e2#;
   pragma Export (C, u00291, "gl__frustumsS");
   u00292 : constant Version_32 := 16#cd53cf3b#;
   pragma Export (C, u00292, "gl__skinned_geometryB");
   u00293 : constant Version_32 := 16#ec459e05#;
   pragma Export (C, u00293, "gl__skinned_geometryS");
   u00294 : constant Version_32 := 16#c62d8c42#;
   pragma Export (C, u00294, "glut_2dB");
   u00295 : constant Version_32 := 16#7eff0cd3#;
   pragma Export (C, u00295, "glut_2dS");
   u00296 : constant Version_32 := 16#e13e5087#;
   pragma Export (C, u00296, "glutB");
   u00297 : constant Version_32 := 16#6ee34171#;
   pragma Export (C, u00297, "glutS");
   u00298 : constant Version_32 := 16#01a73f89#;
   pragma Export (C, u00298, "ada__command_lineB");
   u00299 : constant Version_32 := 16#3cdef8c9#;
   pragma Export (C, u00299, "ada__command_lineS");
   u00300 : constant Version_32 := 16#8225628b#;
   pragma Export (C, u00300, "ada__containers__red_black_treesS");
   u00301 : constant Version_32 := 16#763b4fd7#;
   pragma Export (C, u00301, "graphics_frameratesB");
   u00302 : constant Version_32 := 16#e379c8e5#;
   pragma Export (C, u00302, "graphics_frameratesS");
   u00303 : constant Version_32 := 16#357666d8#;
   pragma Export (C, u00303, "ada__calendar__delaysB");
   u00304 : constant Version_32 := 16#a808adf5#;
   pragma Export (C, u00304, "ada__calendar__delaysS");
   u00305 : constant Version_32 := 16#9bd4ba92#;
   pragma Export (C, u00305, "graphics_structuresS");
   u00306 : constant Version_32 := 16#f2a9860d#;
   pragma Export (C, u00306, "rotationsB");
   u00307 : constant Version_32 := 16#f942fbb5#;
   pragma Export (C, u00307, "rotationsS");
   u00308 : constant Version_32 := 16#621b3e02#;
   pragma Export (C, u00308, "matricesB");
   u00309 : constant Version_32 := 16#48257de2#;
   pragma Export (C, u00309, "matricesS");
   u00310 : constant Version_32 := 16#dd900968#;
   pragma Export (C, u00310, "quaternionsB");
   u00311 : constant Version_32 := 16#504fbd0e#;
   pragma Export (C, u00311, "quaternionsS");
   u00312 : constant Version_32 := 16#3315000b#;
   pragma Export (C, u00312, "vectors_3dB");
   u00313 : constant Version_32 := 16#9351ec73#;
   pragma Export (C, u00313, "vectors_3dS");
   u00314 : constant Version_32 := 16#fe1ffede#;
   pragma Export (C, u00314, "ada__strings__boundedB");
   u00315 : constant Version_32 := 16#89c18940#;
   pragma Export (C, u00315, "ada__strings__boundedS");
   u00316 : constant Version_32 := 16#7ec26662#;
   pragma Export (C, u00316, "ada__strings__superboundedB");
   u00317 : constant Version_32 := 16#da6addee#;
   pragma Export (C, u00317, "ada__strings__superboundedS");
   u00318 : constant Version_32 := 16#35a52d91#;
   pragma Export (C, u00318, "vectors_xdB");
   u00319 : constant Version_32 := 16#4c943a4c#;
   pragma Export (C, u00319, "vectors_xdS");
   u00320 : constant Version_32 := 16#15404f33#;
   pragma Export (C, u00320, "vectors_2d_nS");
   u00321 : constant Version_32 := 16#ae860a75#;
   pragma Export (C, u00321, "vectors_xd_iB");
   u00322 : constant Version_32 := 16#32b570f3#;
   pragma Export (C, u00322, "vectors_xd_iS");
   u00323 : constant Version_32 := 16#99c5812a#;
   pragma Export (C, u00323, "graphics_dataS");
   u00324 : constant Version_32 := 16#9cceee23#;
   pragma Export (C, u00324, "modelsB");
   u00325 : constant Version_32 := 16#670c4a41#;
   pragma Export (C, u00325, "modelsS");
   u00326 : constant Version_32 := 16#407e14ab#;
   pragma Export (C, u00326, "spaceship_pB");
   u00327 : constant Version_32 := 16#de928fde#;
   pragma Export (C, u00327, "spaceship_pS");
   u00328 : constant Version_32 := 16#bfd27222#;
   pragma Export (C, u00328, "sphere_pB");
   u00329 : constant Version_32 := 16#a2350170#;
   pragma Export (C, u00329, "sphere_pS");
   u00330 : constant Version_32 := 16#ed26937f#;
   pragma Export (C, u00330, "vectors_4dS");
   u00331 : constant Version_32 := 16#bf77ef48#;
   pragma Export (C, u00331, "graphics_openglB");
   u00332 : constant Version_32 := 16#380aa6a1#;
   pragma Export (C, u00332, "graphics_openglS");
   u00333 : constant Version_32 := 16#d976e2b4#;
   pragma Export (C, u00333, "ada__numerics__float_randomB");
   u00334 : constant Version_32 := 16#62aa8dd2#;
   pragma Export (C, u00334, "ada__numerics__float_randomS");
   u00335 : constant Version_32 := 16#ec9cfed1#;
   pragma Export (C, u00335, "system__random_numbersB");
   u00336 : constant Version_32 := 16#cb43df61#;
   pragma Export (C, u00336, "system__random_numbersS");
   u00337 : constant Version_32 := 16#650caaea#;
   pragma Export (C, u00337, "system__random_seedB");
   u00338 : constant Version_32 := 16#534b46a0#;
   pragma Export (C, u00338, "system__random_seedS");
   u00339 : constant Version_32 := 16#1d9142a4#;
   pragma Export (C, u00339, "system__val_unsB");
   u00340 : constant Version_32 := 16#2c75fe43#;
   pragma Export (C, u00340, "system__val_unsS");
   u00341 : constant Version_32 := 16#ffa721d2#;
   pragma Export (C, u00341, "globe_3d__stars_skyB");
   u00342 : constant Version_32 := 16#60803aec#;
   pragma Export (C, u00342, "globe_3d__stars_skyS");
   u00343 : constant Version_32 := 16#9df65f1b#;
   pragma Export (C, u00343, "graphics_setupB");
   u00344 : constant Version_32 := 16#ae3cfa3a#;
   pragma Export (C, u00344, "graphics_setupS");
   u00345 : constant Version_32 := 16#03e83d1c#;
   pragma Export (C, u00345, "ada__numerics__elementary_functionsB");
   u00346 : constant Version_32 := 16#edc89b7f#;
   pragma Export (C, u00346, "ada__numerics__elementary_functionsS");
   u00347 : constant Version_32 := 16#748f0b17#;
   pragma Export (C, u00347, "glut__devicesB");
   u00348 : constant Version_32 := 16#c8f38135#;
   pragma Export (C, u00348, "glut__devicesS");
   u00349 : constant Version_32 := 16#da9d8aa7#;
   pragma Export (C, u00349, "glut__windowsB");
   u00350 : constant Version_32 := 16#75e2ce49#;
   pragma Export (C, u00350, "glut__windowsS");
   u00351 : constant Version_32 := 16#1a19ed33#;
   pragma Export (C, u00351, "actorsB");
   u00352 : constant Version_32 := 16#9c19e623#;
   pragma Export (C, u00352, "actorsS");
   u00353 : constant Version_32 := 16#d317e11b#;
   pragma Export (C, u00353, "game_controlB");
   u00354 : constant Version_32 := 16#5097ee25#;
   pragma Export (C, u00354, "game_controlS");
   u00355 : constant Version_32 := 16#c48063ce#;
   pragma Export (C, u00355, "globe_3d__software_anti_aliasingB");
   u00356 : constant Version_32 := 16#41a04269#;
   pragma Export (C, u00356, "globe_3d__software_anti_aliasingS");
   u00357 : constant Version_32 := 16#ecc4ed1f#;
   pragma Export (C, u00357, "keyboardB");
   u00358 : constant Version_32 := 16#e8d894be#;
   pragma Export (C, u00358, "keyboardS");
   u00359 : constant Version_32 := 16#339eba20#;
   pragma Export (C, u00359, "screenshotsB");
   u00360 : constant Version_32 := 16#fb6ddf6f#;
   pragma Export (C, u00360, "screenshotsS");
   u00361 : constant Version_32 := 16#4e9834aa#;
   pragma Export (C, u00361, "swarm_configurationB");
   u00362 : constant Version_32 := 16#58067b51#;
   pragma Export (C, u00362, "swarm_configurationS");
   u00363 : constant Version_32 := 16#844fe575#;
   pragma Export (C, u00363, "swarm_configurationsB");
   u00364 : constant Version_32 := 16#bf53612f#;
   pragma Export (C, u00364, "swarm_configurationsS");
   u00365 : constant Version_32 := 16#eea673f9#;
   pragma Export (C, u00365, "swarm_structures_baseS");
   u00366 : constant Version_32 := 16#118d926e#;
   pragma Export (C, u00366, "generic_protectedB");
   u00367 : constant Version_32 := 16#dac5157f#;
   pragma Export (C, u00367, "generic_protectedS");
   u00368 : constant Version_32 := 16#cc990476#;
   pragma Export (C, u00368, "system__tasking__protected_objects__operationsB");
   u00369 : constant Version_32 := 16#ba36ad85#;
   pragma Export (C, u00369, "system__tasking__protected_objects__operationsS");
   u00370 : constant Version_32 := 16#2db45322#;
   pragma Export (C, u00370, "system__tasking__entry_callsB");
   u00371 : constant Version_32 := 16#c7180c67#;
   pragma Export (C, u00371, "system__tasking__entry_callsS");
   u00372 : constant Version_32 := 16#624cb93c#;
   pragma Export (C, u00372, "system__tasking__rendezvousB");
   u00373 : constant Version_32 := 16#f242aaf9#;
   pragma Export (C, u00373, "system__tasking__rendezvousS");
   u00374 : constant Version_32 := 16#e582cfb0#;
   pragma Export (C, u00374, "swarm_controlB");
   u00375 : constant Version_32 := 16#23efe8fe#;
   pragma Export (C, u00375, "swarm_controlS");
   u00376 : constant Version_32 := 16#84d70551#;
   pragma Export (C, u00376, "swarm_dataS");
   u00377 : constant Version_32 := 16#a3e1fbc0#;
   pragma Export (C, u00377, "swarm_structuresB");
   u00378 : constant Version_32 := 16#87aa0ec8#;
   pragma Export (C, u00378, "swarm_structuresS");
   u00379 : constant Version_32 := 16#5aabc651#;
   pragma Export (C, u00379, "barrier_typeB");
   u00380 : constant Version_32 := 16#94192802#;
   pragma Export (C, u00380, "barrier_typeS");
   u00381 : constant Version_32 := 16#98ccd1c1#;
   pragma Export (C, u00381, "generic_realtime_bufferB");
   u00382 : constant Version_32 := 16#a4c755fe#;
   pragma Export (C, u00382, "generic_realtime_bufferS");
   u00383 : constant Version_32 := 16#710eed06#;
   pragma Export (C, u00383, "vehicle_message_typeS");
   u00384 : constant Version_32 := 16#5da508ef#;
   pragma Export (C, u00384, "vehicle_task_typeB");
   u00385 : constant Version_32 := 16#d9636149#;
   pragma Export (C, u00385, "vehicle_task_typeS");
   u00386 : constant Version_32 := 16#82b6184f#;
   pragma Export (C, u00386, "system__tasking__stagesB");
   u00387 : constant Version_32 := 16#1b9f5506#;
   pragma Export (C, u00387, "system__tasking__stagesS");
   u00388 : constant Version_32 := 16#b00860db#;
   pragma Export (C, u00388, "vehicle_interfaceB");
   u00389 : constant Version_32 := 16#72d0071b#;
   pragma Export (C, u00389, "vehicle_interfaceS");
   u00390 : constant Version_32 := 16#d51f6d70#;
   pragma Export (C, u00390, "system__tasking__async_delaysB");
   u00391 : constant Version_32 := 16#5faa1c14#;
   pragma Export (C, u00391, "system__tasking__async_delaysS");
   u00392 : constant Version_32 := 16#969cb1f9#;
   pragma Export (C, u00392, "system__interrupt_management__operationsB");
   u00393 : constant Version_32 := 16#19b909c9#;
   pragma Export (C, u00393, "system__interrupt_management__operationsS");
   u00394 : constant Version_32 := 16#a6d7988d#;
   pragma Export (C, u00394, "vectors_3d_lfB");
   u00395 : constant Version_32 := 16#abff4c19#;
   pragma Export (C, u00395, "vectors_3d_lfS");
   u00396 : constant Version_32 := 16#1e27852c#;
   pragma Export (C, u00396, "vectors_conversionsB");
   u00397 : constant Version_32 := 16#923ddf49#;
   pragma Export (C, u00397, "vectors_conversionsS");
   u00398 : constant Version_32 := 16#cdecf3e0#;
   pragma Export (C, u00398, "vectors_2dS");
   u00399 : constant Version_32 := 16#3b68dc9f#;
   pragma Export (C, u00399, "vectors_2d_iS");
   u00400 : constant Version_32 := 16#e9217b79#;
   pragma Export (C, u00400, "vectors_2d_pS");
   u00401 : constant Version_32 := 16#878e5819#;
   pragma Export (C, u00401, "swarm_control_concurrent_genericB");
   u00402 : constant Version_32 := 16#7d3a30dd#;
   pragma Export (C, u00402, "swarm_control_concurrent_genericS");

   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  ada.characters%s
   --  ada.characters.latin_1%s
   --  interfaces%s
   --  system%s
   --  system.address_operations%s
   --  system.address_operations%b
   --  system.atomic_counters%s
   --  system.atomic_counters%b
   --  system.exn_llf%s
   --  system.exn_llf%b
   --  system.float_control%s
   --  system.float_control%b
   --  system.img_bool%s
   --  system.img_bool%b
   --  system.img_enum_new%s
   --  system.img_enum_new%b
   --  system.img_int%s
   --  system.img_int%b
   --  system.img_lli%s
   --  system.img_lli%b
   --  system.io%s
   --  system.io%b
   --  system.machine_code%s
   --  system.os_primitives%s
   --  system.os_primitives%b
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  interfaces.c_streams%s
   --  interfaces.c_streams%b
   --  system.powten_table%s
   --  system.restrictions%s
   --  system.restrictions%b
   --  system.storage_elements%s
   --  system.storage_elements%b
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.stack_usage%s
   --  system.stack_usage%b
   --  system.string_hash%s
   --  system.string_hash%b
   --  system.htable%s
   --  system.htable%b
   --  system.strings%s
   --  system.strings%b
   --  system.task_info%s
   --  system.task_info%b
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  system.unsigned_types%s
   --  system.img_llu%s
   --  system.img_llu%b
   --  system.img_uns%s
   --  system.img_uns%b
   --  system.wch_con%s
   --  system.wch_con%b
   --  system.wch_jis%s
   --  system.wch_jis%b
   --  system.wch_cnv%s
   --  system.wch_cnv%b
   --  system.compare_array_unsigned_8%s
   --  system.compare_array_unsigned_8%b
   --  system.concat_2%s
   --  system.concat_2%b
   --  system.concat_3%s
   --  system.concat_3%b
   --  system.concat_4%s
   --  system.concat_4%b
   --  system.concat_5%s
   --  system.concat_5%b
   --  system.concat_6%s
   --  system.concat_6%b
   --  system.concat_7%s
   --  system.concat_7%b
   --  system.concat_8%s
   --  system.concat_8%b
   --  system.traceback%s
   --  system.traceback%b
   --  system.wch_stw%s
   --  system.standard_library%s
   --  system.exceptions_debug%s
   --  system.exceptions_debug%b
   --  ada.exceptions%s
   --  system.wch_stw%b
   --  ada.exceptions.traceback%s
   --  system.secondary_stack%s
   --  system.address_image%s
   --  system.soft_links%s
   --  system.exception_table%s
   --  system.exception_table%b
   --  system.exceptions%s
   --  system.exceptions%b
   --  ada.exceptions.last_chance_handler%s
   --  system.memory%s
   --  system.memory%b
   --  ada.exceptions.traceback%b
   --  system.traceback.symbolic%s
   --  system.traceback.symbolic%b
   --  system.exceptions.machine%s
   --  system.exceptions.machine%b
   --  system.secondary_stack%b
   --  system.address_image%b
   --  system.soft_links.initialize%s
   --  system.soft_links.initialize%b
   --  system.soft_links%b
   --  ada.exceptions.last_chance_handler%b
   --  system.standard_library%b
   --  ada.exceptions%b
   --  ada.command_line%s
   --  ada.command_line%b
   --  ada.containers%s
   --  ada.containers.prime_numbers%s
   --  ada.containers.prime_numbers%b
   --  ada.exceptions.is_null_occurrence%s
   --  ada.exceptions.is_null_occurrence%b
   --  ada.io_exceptions%s
   --  ada.numerics%s
   --  ada.strings%s
   --  interfaces.c%s
   --  interfaces.c%b
   --  interfaces.c.extensions%s
   --  interfaces.c.strings%s
   --  interfaces.c.strings%b
   --  system.case_util%s
   --  system.case_util%b
   --  system.fat_flt%s
   --  system.fat_lflt%s
   --  system.fat_llf%s
   --  ada.numerics.aux%s
   --  ada.numerics.aux%b
   --  ada.numerics.elementary_functions%s
   --  ada.numerics.elementary_functions%b
   --  system.img_real%s
   --  system.img_real%b
   --  system.multiprocessors%s
   --  system.multiprocessors%b
   --  system.os_constants%s
   --  system.os_interface%s
   --  system.os_interface%b
   --  system.interrupt_management%s
   --  system.interrupt_management%b
   --  system.os_lib%s
   --  system.os_lib%b
   --  system.task_primitives%s
   --  system.tasking%s
   --  system.task_primitives.operations%s
   --  system.tasking.debug%s
   --  system.tasking%b
   --  system.task_primitives.operations%b
   --  system.tasking.debug%b
   --  system.val_util%s
   --  system.val_util%b
   --  system.val_llu%s
   --  system.val_llu%b
   --  ada.tags%s
   --  ada.tags%b
   --  ada.streams%s
   --  ada.streams%b
   --  system.communication%s
   --  system.communication%b
   --  system.file_control_block%s
   --  system.finalization_root%s
   --  system.finalization_root%b
   --  ada.finalization%s
   --  ada.containers.helpers%s
   --  ada.containers.helpers%b
   --  ada.containers.hash_tables%s
   --  ada.containers.red_black_trees%s
   --  system.file_io%s
   --  system.file_io%b
   --  ada.streams.stream_io%s
   --  ada.streams.stream_io%b
   --  system.storage_pools%s
   --  system.storage_pools%b
   --  system.finalization_masters%s
   --  system.finalization_masters%b
   --  system.storage_pools.subpools%s
   --  system.storage_pools.subpools.finalization%s
   --  system.storage_pools.subpools%b
   --  system.storage_pools.subpools.finalization%b
   --  system.stream_attributes%s
   --  system.stream_attributes%b
   --  system.val_real%s
   --  system.val_real%b
   --  system.val_uns%s
   --  system.val_uns%b
   --  ada.calendar%s
   --  ada.calendar%b
   --  ada.calendar.delays%s
   --  ada.calendar.delays%b
   --  ada.real_time%s
   --  ada.real_time%b
   --  ada.text_io%s
   --  ada.text_io%b
   --  ada.text_io.generic_aux%s
   --  ada.text_io.generic_aux%b
   --  ada.text_io.float_aux%s
   --  ada.text_io.float_aux%b
   --  system.assertions%s
   --  system.assertions%b
   --  system.bit_ops%s
   --  system.bit_ops%b
   --  ada.strings.maps%s
   --  ada.strings.maps%b
   --  ada.strings.maps.constants%s
   --  ada.characters.handling%s
   --  ada.characters.handling%b
   --  ada.strings.search%s
   --  ada.strings.search%b
   --  ada.strings.fixed%s
   --  ada.strings.fixed%b
   --  ada.strings.superbounded%s
   --  ada.strings.superbounded%b
   --  ada.strings.bounded%s
   --  ada.strings.bounded%b
   --  ada.strings.unbounded%s
   --  ada.strings.unbounded%b
   --  ada.strings.unbounded.hash%s
   --  ada.strings.unbounded.hash%b
   --  system.interrupt_management.operations%s
   --  system.interrupt_management.operations%b
   --  system.pool_global%s
   --  system.pool_global%b
   --  system.random_seed%s
   --  system.random_seed%b
   --  system.random_numbers%s
   --  system.random_numbers%b
   --  ada.numerics.float_random%s
   --  ada.numerics.float_random%b
   --  system.soft_links.tasking%s
   --  system.soft_links.tasking%b
   --  system.strings.stream_ops%s
   --  system.strings.stream_ops%b
   --  system.tasking.initialization%s
   --  system.tasking.task_attributes%s
   --  system.tasking.initialization%b
   --  system.tasking.task_attributes%b
   --  system.tasking.protected_objects%s
   --  system.tasking.protected_objects%b
   --  system.tasking.protected_objects.entries%s
   --  system.tasking.protected_objects.entries%b
   --  system.tasking.queuing%s
   --  system.tasking.queuing%b
   --  system.tasking.utilities%s
   --  system.tasking.utilities%b
   --  ada.task_identification%s
   --  ada.task_identification%b
   --  system.tasking.entry_calls%s
   --  system.tasking.rendezvous%s
   --  system.tasking.protected_objects.operations%s
   --  system.tasking.protected_objects.operations%b
   --  system.tasking.entry_calls%b
   --  system.tasking.rendezvous%b
   --  system.tasking.stages%s
   --  system.tasking.stages%b
   --  system.tasking.async_delays%s
   --  system.tasking.async_delays%b
   --  barrier_type%s
   --  barrier_type%b
   --  bzip2%s
   --  bzip2%b
   --  exceptions%s
   --  exceptions%b
   --  generic_protected%s
   --  generic_protected%b
   --  generic_realtime_buffer%s
   --  generic_realtime_buffer%b
   --  gl%s
   --  gl.extended%s
   --  gl%b
   --  gl.buffer%s
   --  gl.buffer%b
   --  gl.io%s
   --  gl.io%b
   --  gl.materials%s
   --  gl.materials%b
   --  gl.math%s
   --  gl.math%b
   --  gl.geometry%s
   --  gl.geometry%b
   --  gl.frustums%s
   --  gl.frustums%b
   --  glu%s
   --  glu%b
   --  gl.errors%s
   --  gl.errors%b
   --  gl.buffer.general%s
   --  gl.buffer.general%b
   --  gl.textures%s
   --  gl.textures%b
   --  gl.buffer.texture_coords%s
   --  gl.buffer.texture_coords%b
   --  gl.skins%s
   --  gl.skins%b
   --  gl.skinned_geometry%s
   --  gl.skinned_geometry%b
   --  glut%s
   --  glut%b
   --  glut_2d%s
   --  glut_2d%b
   --  real_type%s
   --  generic_sliding_statistics%s
   --  generic_sliding_statistics%b
   --  graphics_framerates%s
   --  graphics_framerates%b
   --  matrices%s
   --  matrices%b
   --  quaternions%s
   --  quaternions%b
   --  screenshots%s
   --  screenshots%b
   --  vectors_xd%s
   --  vectors_xd%b
   --  vectors_2d%s
   --  vectors_3d%s
   --  vectors_3d%b
   --  rotations%s
   --  rotations%b
   --  vectors_3d_lf%s
   --  vectors_3d_lf%b
   --  vectors_4d%s
   --  vectors_xd_i%s
   --  vectors_xd_i%b
   --  vectors_2d_i%s
   --  vectors_2d_n%s
   --  vectors_2d_p%s
   --  vectors_conversions%s
   --  vectors_conversions%b
   --  zip_streams%s
   --  zip%s
   --  zip_streams%b
   --  zip.headers%s
   --  zip.headers%b
   --  zip%b
   --  zip.crc%s
   --  zip.crc%b
   --  unzip%s
   --  unzip.decompress%s
   --  unzip.decompress.huffman%s
   --  unzip.decompress.huffman%b
   --  unzip%b
   --  unzip.decompress%b
   --  unzip.streams%s
   --  unzip.streams%b
   --  globe_3d%s
   --  globe_3d.textures%s
   --  globe_3d.portals%s
   --  globe_3d.options%s
   --  globe_3d.options%b
   --  globe_3d.math%s
   --  globe_3d.math%b
   --  globe_3d.textures%b
   --  globe_3d%b
   --  globe_3d.portals%b
   --  globe_3d.software_anti_aliasing%s
   --  globe_3d.software_anti_aliasing%b
   --  globe_3d.stars_sky%s
   --  globe_3d.stars_sky%b
   --  glut.devices%s
   --  game_control%s
   --  game_control%b
   --  actors%s
   --  actors%b
   --  glut.windows%s
   --  glut.windows%b
   --  glut.devices%b
   --  graphics_structures%s
   --  graphics_configuration%s
   --  keyboard%s
   --  keyboard%b
   --  spaceship_p%s
   --  spaceship_p%b
   --  sphere_p%s
   --  sphere_p%b
   --  models%s
   --  models%b
   --  graphics_data%s
   --  graphics_setup%s
   --  graphics_setup%b
   --  graphics_opengl%s
   --  graphics_opengl%b
   --  swarm_structures_base%s
   --  swarm_configurations%s
   --  swarm_configurations%b
   --  swarm_configuration%s
   --  swarm_configuration%b
   --  vehicle_task_type%s
   --  vehicle_message_type%s
   --  swarm_structures%s
   --  swarm_structures%b
   --  swarm_data%s
   --  swarm_control%s
   --  swarm_control%b
   --  vehicle_interface%s
   --  vehicle_interface%b
   --  vehicle_task_type%b
   --  swarm_control_concurrent_generic%s
   --  swarm_control_concurrent_generic%b
   --  callback_procedures%s
   --  callback_procedures%b
   --  swarm%b
   --  END ELABORATION ORDER

end ada_main;
