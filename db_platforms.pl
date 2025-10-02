% --- db_platforms.pl ---
% platform(Trademark, SerialID, Year, RAM_GB, CPUManufacturer, Cores, HDD_GB, Type, GPUManufacturer, VRAM_GB).

platform(asus, p001, 2020, 8, intel, 4, 512, laptop, nvidia, 2).
platform(asus, p002, 2022, 16, amd, 8, 1024, pc, amd_gpu, 4).
platform(lenovo, p003, 2019, 4, intel, 2, 256, laptop, intel_graphics, 0.5).
platform(hp, p004, 2023, 8, amd, 6, 512, laptop, nvidia, 2).
platform(acer, p005, 2021, 2, intel, 2, 128, tablet, intel_graphics, 0.5).
platform(dell, p006, 2022, 32, intel, 12, 2048, pc, nvidia, 8).
platform(asus, p007, 2018, 4, amd, 4, 500, laptop, amd_gpu, 1).
platform(apple, p008, 2023, 8, apple_m1, 8, 512, laptop, apple_gpu, 4).
platform(huawei, p009, 2020, 3, amd, 2, 64, tablet, mali, 0.5).
platform(lenovo, p010, 2024, 16, intel, 8, 512, pc, nvidia, 6).
platform(asus, p011, 2022, 2, amd, 2, 32, tablet, amd_gpu, 0.5).
platform(acer, p012, 2017, 8, intel, 4, 256, laptop, intel_graphics, 1).
platform(hp, p013, 2021, 16, amd, 8, 512, pc, amd_gpu, 4).
platform(dell, p014, 2019, 1, intel, 2, 32, tablet, intel_graphics, 0.25).
platform(asus, p015, 2023, 64, amd, 16, 2048, pc, nvidia, 12).
platform(hp, p016, 2020, 8, intel, 4, 128, laptop, nvidia, 2).
platform(lenovo, p017, 2022, 4, amd, 4, 128, laptop, amd_gpu, 1).
platform(dell, p018, 2021, 8, intel, 6, 512, laptop, nvidia, 3).
platform(acer, p019, 2024, 32, amd, 12, 1024, pc, amd_gpu, 8).
platform(apple, p020, 2020, 8, apple_m1, 8, 256, laptop, apple_gpu, 4).
platform(asus, p021, 2016, 2, intel, 1, 64, tablet, intel_graphics, 0.25).
platform(lenovo, p022, 2018, 4, amd, 2, 128, tablet, amd_gpu, 0.5).
platform(hp, p023, 2023, 16, intel, 8, 1024, pc, nvidia, 6).
platform(dell, p024, 2024, 8, amd, 6, 256, laptop, amd_gpu, 2).
platform(asus, p025, 2022, 4, intel, 4, 128, laptop, intel_graphics, 1).
platform(acer, p026, 2019, 2, amd, 2, 64, tablet, mali, 0.5).
platform(lenovo, p027, 2020, 8, intel, 4, 500, pc, nvidia, 2).
platform(hp, p028, 2021, 3, amd, 2, 32, tablet, amd_gpu, 0.5).
platform(dell, p029, 2017, 16, intel, 8, 1000, pc, nvidia, 4).
platform(asus, p030, 2024, 128, amd, 32, 4096, pc, nvidia, 24).
platform(hp, p031, 2022, 4, intel, 4, 128, laptop, intel_graphics, 1).
platform(acer, p032, 2020, 6, amd, 4, 256, laptop, amd_gpu, 2).
platform(lenovo, p033, 2023, 8, intel, 6, 512, laptop, nvidia, 3).
platform(dell, p034, 2015, 2, intel, 1, 32, tablet, intel_graphics, 0.25).
platform(asus, p035, 2021, 12, amd, 8, 1000, pc, amd_gpu, 6).
platform(apple, p036, 2024, 16, apple_m2, 8, 1024, laptop, apple_gpu, 8).
platform(huawei, p037, 2023, 4, amd, 4, 256, laptop, amd_gpu, 2).
platform(acer, p038, 2018, 8, intel, 4, 512, pc, nvidia, 2).
platform(lenovo, p039, 2019, 2, amd, 2, 32, tablet, amd_gpu, 0.5).
platform(hp, p040, 2024, 32, intel, 16, 2048, pc, nvidia, 10).
platform(dell, p041, 2022, 8, amd, 8, 512, laptop, amd_gpu, 4).
platform(asus, p042, 2020, 4, intel, 4, 256, laptop, intel_graphics, 1).
platform(acer, p043, 2021, 16, amd, 12, 1024, pc, amd_gpu, 6).
platform(lenovo, p044, 2016, 1, intel, 1, 16, tablet, intel_graphics, 0.25).
platform(hp, p045, 2018, 8, amd, 4, 500, laptop, amd_gpu, 2).
platform(dell, p046, 2023, 64, intel, 24, 4096, pc, nvidia, 16).
platform(asus, p047, 2024, 8, amd, 8, 512, laptop, amd_gpu, 4).
platform(acer, p048, 2022, 3, intel, 2, 64, tablet, intel_graphics, 0.5).
platform(lenovo, p049, 2021, 4, amd, 4, 256, laptop, amd_gpu, 1).
platform(hp, p050, 2020, 2, intel, 2, 32, tablet, intel_graphics, 0.25).

% ----------------------
% Consultas requeridas
% ----------------------

% 1) ¿Qué plataformas tienen CPU AMD adquiridas después de 2021?
amd_after_2021(S) :-
    findall((Trademark,SerialID,Year,RAM_GB,CPUManufacturer), 
            (platform(Trademark,SerialID,Year,RAM_GB,amd,_,_,_,_,_), Year > 2021),
            S).

% 2) ¿Qué plataformas son tablets con más de 2GB de RAM instalado?
tablets_more_than_2gb(L) :-
    findall((Trademark,SerialID,RAM_GB),
            (platform(Trademark,SerialID,_,RAM_GB,_,_,_,tablet,_,_), RAM_GB > 2),
            L).

% 3) ¿Qué plataformas tienen discos entre 32GB y 512GB?
hdd_between_32_512(L) :-
    findall((Trademark,SerialID,HDD_GB),
            (platform(Trademark,SerialID,_,_,_,_,HDD_GB,_,_,_), HDD_GB >= 32, HDD_GB =< 512),
            L).

% 4) ¿Cuántas plataformas son de ASUS?
count_asus(N) :-
    findall(Serial, platform(asus, Serial, _, _, _, _, _, _, _, _), L),
    length(L, N).

% 5) ¿Cuántas plataformas son Laptops con más de 4GB de RAM y menos de 512GB de disco?
count_laptops_ram_hdd(N) :-
    findall(Serial, (platform(_, Serial, _, RAM, _, _, HDD, laptop, _, _), RAM > 4, HDD < 512), L),
    length(L, N).

% ----------------------
% Consultas adicionales (4 consultas complejas)
% ----------------------

% A) Computadoras con >=8 núcleos y >=4 GB VRAM adquiridas antes de 2022
high_perf_before_2022(L) :-
    findall((Trademark,SerialID,Year,Cores,VRAM_GB),
            (platform(Trademark,SerialID,Year,_,_,Cores,_,_,_,VRAM_GB),
             Cores >= 8, VRAM_GB >= 4, Year < 2022),
            L).

% B) Plataformas AMD o ASUS con menos de 128GB de disco pero con RAM >=4
amd_or_asus_small_hdd(L) :-
    findall((Trademark,SerialID,RAM,G, HDD),
            (platform(Trademark,SerialID,_,RAM,CPU,_,HDD,_,_,_),
             (CPU == amd ; Trademark == asus),
             HDD < 128, RAM >= 4),
            L).

% C) Plataformas adquiridas entre 2020 y 2023 con al menos 6 núcleos, ordenadas por año ascendente
midrange_2020_2023(L) :-
    findall((Year,Trademark,SerialID,Cores),
            (platform(Trademark,SerialID,Year,_,_,Cores,_,_,_,_), Year >= 2020, Year =< 2023, Cores >= 6),
            Unsorted),
    sort(Unsorted, L).

% D) Top N plataformas con más VRAM (argumento N)
top_n_by_vram(N, TopN) :-
    findall((VRAM,Trademark,SerialID),
            (platform(Trademark,SerialID,_,_,_,_,_,_,_,VRAM)),
            L),
    sort(0, @>=, L, Sorted), % sort descending by VRAM
    length(Head, N),
    append(Head, _, Sorted),
    TopN = Head.
