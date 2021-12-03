Require Import  Coq.Lists.List.
Require Import Coq.Init.Nat.

Open Scope nat_scope.

Open Scope list_scope.

Import ListNotations.

Definition data := [ 127 ; 147 ; 148 ; 147 ; 146 ; 153 ; 154 ; 167 ; 184 ; 181 ; 194 ; 183 ; 188 ; 195 ; 193 ; 207 ; 208 ; 220 ; 222 ; 234 ; 226 ; 247 ; 248 ; 253 ; 257 ; 250 ; 251 ; 285 ; 293 ; 294 ; 314 ; 311 ; 305 ; 312 ; 318 ; 321 ; 322 ; 344 ; 353 ; 354 ; 383 ; 391 ; 393 ; 394 ; 369 ; 373 ; 377 ; 378 ; 385 ; 388 ; 400 ; 402 ; 414 ; 422 ; 435 ; 414 ; 422 ; 436 ; 446 ; 455 ; 463 ; 478 ; 483 ; 482 ; 499 ; 495 ; 501 ; 506 ; 507 ; 509 ; 529 ; 522 ; 526 ; 527 ; 530 ; 529 ; 535 ; 536 ; 520 ; 525 ; 516 ; 518 ; 524 ; 525 ; 519 ; 520 ; 525 ; 531 ; 539 ; 551 ; 555 ; 579 ; 583 ; 594 ; 593 ; 594 ; 611 ; 610 ; 613 ; 616 ; 630 ; 637 ; 645 ; 641 ; 644 ; 646 ; 659 ; 671 ; 672 ; 676 ; 678 ; 682 ; 686 ; 688 ; 694 ; 720 ; 725 ; 735 ; 741 ; 742 ; 748 ; 745 ; 747 ; 752 ; 775 ; 776 ; 778 ; 777 ; 791 ; 797 ; 799 ; 800 ; 801 ; 806 ; 802 ; 804 ; 808 ; 817 ; 815 ; 817 ; 825 ; 830 ; 838 ; 839 ; 848 ; 849 ; 856 ; 861 ; 868 ; 875 ; 881 ; 875 ; 877 ; 858 ; 867 ; 861 ; 865 ; 855 ; 856 ; 857 ; 858 ; 859 ; 863 ; 866 ; 860 ; 871 ; 884 ; 882 ; 893 ; 904 ; 905 ; 917 ; 938 ; 951 ; 957 ; 958 ; 963 ; 967 ; 979 ; 980 ; 979 ; 980 ; 995 ; 994 ; 992 ; 994 ; 996 ; 998 ; 1007 ; 992 ; 1026 ; 1027 ; 1019 ; 1024 ; 1023 ; 1039 ; 1026 ; 1038 ; 1045 ; 1050 ; 1059 ; 1056 ; 1059 ; 1060 ; 1057 ; 1058 ; 1060 ; 1072 ; 1092 ; 1101 ; 1110 ; 1116 ; 1155 ; 1166 ; 1187 ; 1189 ; 1190 ; 1202 ; 1207 ; 1208 ; 1224 ; 1239 ; 1237 ; 1243 ; 1242 ; 1261 ; 1284 ; 1283 ; 1284 ; 1283 ; 1298 ; 1287 ; 1283 ; 1280 ; 1277 ; 1280 ; 1309 ; 1314 ; 1315 ; 1321 ; 1323 ; 1324 ; 1332 ; 1356 ; 1358 ; 1360 ; 1363 ; 1364 ; 1365 ; 1367 ; 1366 ; 1382 ; 1398 ; 1400 ; 1407 ; 1421 ; 1444 ; 1449 ; 1461 ; 1464 ; 1465 ; 1471 ; 1475 ; 1466 ; 1475 ; 1476 ; 1479 ; 1494 ; 1500 ; 1519 ; 1521 ; 1531 ; 1539 ; 1533 ; 1544 ; 1547 ; 1561 ; 1562 ; 1561 ; 1574 ; 1580 ; 1581 ; 1588 ; 1589 ; 1591 ; 1604 ; 1600 ; 1601 ; 1606 ; 1607 ; 1614 ; 1599 ; 1604 ; 1606 ; 1605 ; 1611 ; 1612 ; 1615 ; 1616 ; 1618 ; 1619 ; 1621 ; 1624 ; 1639 ; 1643 ; 1637 ; 1644 ; 1645 ; 1644 ; 1639 ; 1652 ; 1665 ; 1677 ; 1683 ; 1687 ; 1694 ; 1700 ; 1706 ; 1696 ; 1691 ; 1727 ; 1728 ; 1732 ; 1739 ; 1751 ; 1749 ; 1762 ; 1774 ; 1773 ; 1769 ; 1770 ; 1774 ; 1766 ; 1767 ; 1775 ; 1759 ; 1760 ; 1759 ; 1770 ; 1776 ; 1777 ; 1781 ; 1782 ; 1783 ; 1784 ; 1785 ; 1796 ; 1805 ; 1807 ; 1808 ; 1809 ; 1810 ; 1806 ; 1803 ; 1805 ; 1806 ; 1808 ; 1807 ; 1794 ; 1796 ; 1818 ; 1824 ; 1827 ; 1830 ; 1818 ; 1831 ; 1834 ; 1844 ; 1846 ; 1857 ; 1865 ; 1866 ; 1888 ; 1890 ; 1891 ; 1895 ; 1904 ; 1912 ; 1910 ; 1911 ; 1931 ; 1929 ; 1933 ; 1944 ; 1946 ; 1958 ; 1968 ; 1982 ; 1983 ; 1980 ; 1957 ; 1961 ; 1968 ; 1966 ; 1968 ; 1970 ; 1975 ; 1978 ; 1983 ; 1991 ; 1995 ; 1997 ; 2017 ; 2018 ; 2022 ; 2025 ; 2038 ; 2039 ; 2042 ; 2068 ; 2065 ; 2068 ; 2070 ; 2071 ; 2079 ; 2081 ; 2084 ; 2085 ; 2086 ; 2100 ; 2104 ; 2106 ; 2107 ; 2116 ; 2125 ; 2129 ; 2131 ; 2132 ; 2133 ; 2141 ; 2128 ; 2121 ; 2113 ; 2103 ; 2120 ; 2117 ; 2119 ; 2120 ; 2135 ; 2136 ; 2148 ; 2151 ; 2133 ; 2134 ; 2140 ; 2150 ; 2166 ; 2167 ; 2169 ; 2172 ; 2175 ; 2178 ; 2187 ; 2188 ; 2189 ; 2186 ; 2187 ; 2206 ; 2239 ; 2238 ; 2244 ; 2238 ; 2239 ; 2207 ; 2208 ; 2209 ; 2210 ; 2211 ; 2210 ; 2217 ; 2219 ; 2220 ; 2222 ; 2224 ; 2229 ; 2234 ; 2237 ; 2234 ; 2240 ; 2237 ; 2239 ; 2245 ; 2244 ; 2245 ; 2239 ; 2238 ; 2263 ; 2266 ; 2269 ; 2278 ; 2291 ; 2292 ; 2295 ; 2296 ; 2298 ; 2299 ; 2306 ; 2310 ; 2321 ; 2326 ; 2335 ; 2338 ; 2339 ; 2340 ; 2345 ; 2350 ; 2354 ; 2355 ; 2374 ; 2373 ; 2379 ; 2381 ; 2396 ; 2394 ; 2400 ; 2405 ; 2411 ; 2412 ; 2423 ; 2426 ; 2431 ; 2432 ; 2428 ; 2429 ; 2444 ; 2459 ; 2470 ; 2454 ; 2458 ; 2460 ; 2464 ; 2468 ; 2482 ; 2483 ; 2487 ; 2509 ; 2505 ; 2507 ; 2487 ; 2504 ; 2499 ; 2508 ; 2540 ; 2550 ; 2551 ; 2554 ; 2555 ; 2537 ; 2541 ; 2529 ; 2525 ; 2533 ; 2534 ; 2535 ; 2536 ; 2511 ; 2518 ; 2537 ; 2536 ; 2549 ; 2550 ; 2563 ; 2594 ; 2597 ; 2602 ; 2608 ; 2619 ; 2638 ; 2642 ; 2645 ; 2649 ; 2654 ; 2667 ; 2672 ; 2689 ; 2710 ; 2713 ; 2730 ; 2726 ; 2731 ; 2739 ; 2733 ; 2722 ; 2720 ; 2722 ; 2723 ; 2724 ; 2733 ; 2743 ; 2759 ; 2743 ; 2741 ; 2742 ; 2751 ; 2757 ; 2759 ; 2761 ; 2771 ; 2810 ; 2805 ; 2815 ; 2820 ; 2822 ; 2825 ; 2822 ; 2836 ; 2838 ; 2839 ; 2837 ; 2845 ; 2850 ; 2853 ; 2879 ; 2881 ; 2886 ; 2888 ; 2907 ; 2908 ; 2909 ; 2911 ; 2924 ; 2925 ; 2928 ; 2929 ; 2930 ; 2946 ; 2947 ; 2949 ; 2954 ; 2936 ; 2944 ; 2947 ; 2953 ; 2954 ; 2956 ; 2971 ; 2976 ; 2977 ; 2968 ; 2967 ; 2972 ; 2973 ; 2977 ; 2970 ; 2975 ; 2987 ; 2997 ; 2996 ; 2998 ; 2993 ; 2996 ; 2997 ; 2999 ; 3031 ; 3028 ; 3039 ; 3043 ; 3045 ; 3026 ; 3028 ; 3036 ; 3044 ; 3046 ; 3019 ; 3023 ; 3020 ; 3021 ; 3023 ; 3025 ; 3037 ; 3038 ; 3039 ; 3062 ; 3063 ; 3081 ; 3086 ; 3081 ; 3082 ; 3083 ; 3098 ; 3105 ; 3106 ; 3108 ; 3110 ; 3107 ; 3116 ; 3125 ; 3139 ; 3137 ; 3143 ; 3144 ; 3145 ; 3159 ; 3162 ; 3172 ; 3173 ; 3172 ; 3173 ; 3174 ; 3182 ; 3183 ; 3177 ; 3185 ; 3175 ; 3178 ; 3179 ; 3195 ; 3210 ; 3201 ; 3205 ; 3202 ; 3203 ; 3191 ; 3188 ; 3197 ; 3198 ; 3206 ; 3215 ; 3233 ; 3240 ; 3243 ; 3253 ; 3254 ; 3264 ; 3267 ; 3289 ; 3267 ; 3271 ; 3280 ; 3290 ; 3308 ; 3307 ; 3308 ; 3316 ; 3320 ; 3311 ; 3318 ; 3331 ; 3343 ; 3357 ; 3358 ; 3360 ; 3389 ; 3402 ; 3403 ; 3405 ; 3407 ; 3420 ; 3380 ; 3372 ; 3374 ; 3380 ; 3357 ; 3356 ; 3361 ; 3356 ; 3364 ; 3365 ; 3366 ; 3367 ; 3386 ; 3387 ; 3388 ; 3389 ; 3406 ; 3407 ; 3409 ; 3413 ; 3416 ; 3441 ; 3443 ; 3441 ; 3442 ; 3446 ; 3448 ; 3454 ; 3453 ; 3457 ; 3461 ; 3467 ; 3459 ; 3469 ; 3482 ; 3497 ; 3490 ; 3497 ; 3495 ; 3494 ; 3495 ; 3496 ; 3498 ; 3494 ; 3510 ; 3515 ; 3535 ; 3536 ; 3535 ; 3544 ; 3562 ; 3557 ; 3546 ; 3553 ; 3557 ; 3558 ; 3555 ; 3562 ; 3571 ; 3573 ; 3575 ; 3591 ; 3590 ; 3591 ; 3592 ; 3598 ; 3600 ; 3608 ; 3609 ; 3648 ; 3653 ; 3654 ; 3657 ; 3655 ; 3657 ; 3660 ; 3669 ; 3670 ; 3671 ; 3667 ; 3669 ; 3674 ; 3676 ; 3683 ; 3687 ; 3688 ; 3699 ; 3720 ; 3719 ; 3720 ; 3726 ; 3729 ; 3734 ; 3737 ; 3757 ; 3767 ; 3773 ; 3774 ; 3778 ; 3780 ; 3779 ; 3777 ; 3781 ; 3782 ; 3801 ; 3800 ; 3798 ; 3802 ; 3803 ; 3806 ; 3817 ; 3819 ; 3834 ; 3837 ; 3838 ; 3839 ; 3837 ; 3846 ; 3850 ; 3851 ; 3868 ; 3865 ; 3890 ; 3893 ; 3895 ; 3889 ; 3884 ; 3886 ; 3887 ; 3884 ; 3886 ; 3903 ; 3905 ; 3906 ; 3909 ; 3911 ; 3922 ; 3927 ; 3943 ; 3953 ; 3959 ; 3961 ; 3960 ; 3954 ; 3933 ; 3932 ; 3939 ; 3941 ; 3942 ; 3941 ; 3950 ; 3951 ; 3971 ; 3947 ; 3953 ; 3962 ; 3996 ; 4017 ; 4032 ; 4036 ; 4040 ; 4067 ; 4068 ; 4073 ; 4096 ; 4098 ; 4097 ; 4093 ; 4115 ; 4118 ; 4119 ; 4144 ; 4148 ; 4149 ; 4161 ; 4173 ; 4177 ; 4182 ; 4185 ; 4186 ; 4196 ; 4184 ; 4196 ; 4198 ; 4210 ; 4214 ; 4210 ; 4217 ; 4195 ; 4206 ; 4192 ; 4193 ; 4205 ; 4208 ; 4184 ; 4203 ; 4204 ; 4205 ; 4211 ; 4205 ; 4206 ; 4182 ; 4193 ; 4195 ; 4196 ; 4212 ; 4213 ; 4216 ; 4215 ; 4222 ; 4232 ; 4230 ; 4251 ; 4255 ; 4256 ; 4271 ; 4281 ; 4279 ; 4292 ; 4295 ; 4301 ; 4302 ; 4303 ; 4285 ; 4288 ; 4292 ; 4294 ; 4296 ; 4299 ; 4282 ; 4285 ; 4290 ; 4318 ; 4331 ; 4340 ; 4335 ; 4329 ; 4335 ; 4347 ; 4379 ; 4388 ; 4375 ; 4384 ; 4411 ; 4415 ; 4416 ; 4418 ; 4416 ; 4428 ; 4430 ; 4431 ; 4432 ; 4420 ; 4436 ; 4439 ; 4446 ; 4458 ; 4459 ; 4472 ; 4468 ; 4493 ; 4495 ; 4468 ; 4469 ; 4474 ; 4475 ; 4502 ; 4505 ; 4516 ; 4522 ; 4518 ; 4519 ; 4532 ; 4535 ; 4540 ; 4534 ; 4530 ; 4534 ; 4535 ; 4538 ; 4544 ; 4543 ; 4546 ; 4543 ; 4545 ; 4548 ; 4550 ; 4557 ; 4559 ; 4560 ; 4555 ; 4569 ; 4576 ; 4579 ; 4597 ; 4596 ; 4603 ; 4597 ; 4596 ; 4620 ; 4619 ; 4617 ; 4602 ; 4605 ; 4606 ; 4612 ; 4625 ; 4626 ; 4651 ; 4647 ; 4650 ; 4649 ; 4668 ; 4674 ; 4686 ; 4687 ; 4695 ; 4723 ; 4726 ; 4739 ; 4752 ; 4754 ; 4765 ; 4781 ; 4809 ; 4797 ; 4799 ; 4797 ; 4796 ; 4799 ; 4802 ; 4804 ; 4807 ; 4808 ; 4809 ; 4810 ; 4811 ; 4820 ; 4828 ; 4829 ; 4830 ; 4833 ; 4825 ; 4829 ; 4830 ; 4839 ; 4844 ; 4848 ; 4849 ; 4858 ; 4882 ; 4877 ; 4878 ; 4882 ; 4883 ; 4869 ; 4871 ; 4872 ; 4876 ; 4879 ; 4880 ; 4878 ; 4880 ; 4899 ; 4921 ; 4922 ; 4919 ; 4920 ; 4915 ; 4916 ; 4917 ; 4938 ; 4939 ; 4940 ; 4938 ; 4941 ; 4943 ; 4944 ; 4947 ; 4967 ; 4968 ; 4981 ; 4982 ; 4983 ; 4993 ; 4994 ; 4995 ; 5000 ; 5001 ; 5007 ; 5008 ; 5007 ; 5000 ; 5017 ; 5026 ; 5040 ; 5048 ; 5050 ; 5046 ; 5084 ; 5083 ; 5089 ; 5097 ; 5101 ; 5061 ; 5060 ; 5078 ; 5079 ; 5095 ; 5096 ; 5102 ; 5129 ; 5126 ; 5127 ; 5125 ; 5127 ; 5128 ; 5123 ; 5129 ; 5132 ; 5146 ; 5156 ; 5157 ; 5182 ; 5198 ; 5201 ; 5208 ; 5211 ; 5212 ; 5215 ; 5217 ; 5200 ; 5204 ; 5205 ; 5207 ; 5212 ; 5218 ; 5206 ; 5207 ; 5214 ; 5220 ; 5230 ; 5236 ; 5241 ; 5240 ; 5245 ; 5246 ; 5242 ; 5240 ; 5239 ; 5242 ; 5232 ; 5230 ; 5225 ; 5232 ; 5226 ; 5221 ; 5224 ; 5234 ; 5240 ; 5263 ; 5268 ; 5301 ; 5303 ; 5310 ; 5311 ; 5332 ; 5337 ; 5338 ; 5339 ; 5324 ; 5341 ; 5346 ; 5323 ; 5324 ; 5325 ; 5329 ; 5330 ; 5331 ; 5338 ; 5360 ; 5361 ; 5371 ; 5373 ; 5362 ; 5375 ; 5387 ; 5393 ; 5387 ; 5396 ; 5417 ; 5418 ; 5421 ; 5422 ; 5404 ; 5405 ; 5412 ; 5414 ; 5413 ; 5444 ; 5445 ; 5446 ; 5447 ; 5453 ; 5463 ; 5473 ; 5478 ; 5480 ; 5484 ; 5479 ; 5489 ; 5494 ; 5484 ; 5506 ; 5500 ; 5510 ; 5512 ; 5519 ; 5520 ; 5518 ; 5523 ; 5524 ; 5529 ; 5533 ; 5520 ; 5521 ; 5522 ; 5521 ; 5530 ; 5535 ; 5550 ; 5551 ; 5544 ; 5551 ; 5552 ; 5549 ; 5550 ; 5551 ; 5547 ; 5589 ; 5603 ; 5612 ; 5614 ; 5623 ; 5622 ; 5624 ; 5617 ; 5639 ; 5641 ; 5627 ; 5630 ; 5634 ; 5641 ; 5646 ; 5648 ; 5655 ; 5679 ; 5665 ; 5666 ; 5669 ; 5687 ; 5688 ; 5692 ; 5690 ; 5688 ; 5691 ; 5695 ; 5696 ; 5697 ; 5699 ; 5698 ; 5715 ; 5716 ; 5717 ; 5744 ; 5741 ; 5750 ; 5743 ; 5712 ; 5716 ; 5717 ; 5743 ; 5746 ; 5749 ; 5751 ; 5752 ; 5761 ; 5767 ; 5769 ; 5784 ; 5785 ; 5786 ; 5784 ; 5786 ; 5787 ; 5790 ; 5799 ; 5807 ; 5808 ; 5826 ; 5828 ; 5826 ; 5837 ; 5856 ; 5876 ; 5877 ; 5879 ; 5887 ; 5890 ; 5892 ; 5894 ; 5911 ; 5922 ; 5935 ; 5936 ; 5942 ; 5950 ; 5966 ; 5985 ; 5986 ; 5976 ; 5979 ; 5976 ; 5984 ; 5950 ; 5951 ; 5955 ; 5964 ; 5965 ; 5969 ; 5974 ; 5990 ; 6008 ; 6038 ; 6045 ; 6051 ; 6060 ; 6066 ; 6070 ; 6105 ; 6120 ; 6121 ; 6127 ; 6149 ; 6150 ; 6157 ; 6162 ; 6163 ; 6165 ; 6185 ; 6186 ; 6190 ; 6191 ; 6218 ; 6235 ; 6250 ; 6231 ; 6239 ; 6229 ; 6237 ; 6240 ; 6241 ; 6243 ; 6249 ; 6264 ; 6251 ; 6257 ; 6276 ; 6279 ; 6271 ; 6290 ; 6292 ; 6290 ; 6293 ; 6294 ; 6287 ; 6289 ; 6290 ; 6291 ; 6293 ; 6294 ; 6300 ; 6295 ; 6296 ; 6303 ; 6332 ; 6334 ; 6333 ; 6340 ; 6341 ; 6344 ; 6345 ; 6346 ; 6352 ; 6356 ; 6355 ; 6368 ; 6370 ; 6361 ; 6362 ; 6359 ; 6361 ; 6362 ; 6368 ; 6364 ; 6373 ; 6374 ; 6383 ; 6384 ; 6385 ; 6384 ; 6389 ; 6388 ; 6389 ; 6404 ; 6410 ; 6415 ; 6407 ; 6406 ; 6405 ; 6402 ; 6407 ; 6413 ; 6412 ; 6398 ; 6384 ; 6386 ; 6407 ; 6408 ; 6409 ; 6408 ; 6418 ; 6415 ; 6416 ; 6418 ; 6427 ; 6434 ; 6458 ; 6455 ; 6456 ; 6457 ; 6458 ; 6468 ; 6463 ; 6454 ; 6455 ; 6459 ; 6460 ; 6464 ; 6471 ; 6463 ; 6480 ; 6467 ; 6468 ; 6470 ; 6462 ; 6463 ; 6464 ; 6462 ; 6461 ; 6463 ; 6471 ; 6482 ; 6459 ; 6460 ; 6462 ; 6469 ; 6484 ; 6479 ; 6481 ; 6492 ; 6502 ; 6477 ; 6483 ; 6486 ; 6488 ; 6466 ; 6467 ; 6463 ; 6444 ; 6464 ; 6466 ; 6482 ; 6497 ; 6498 ; 6511 ; 6512 ; 6517 ; 6522 ; 6521 ; 6524 ; 6547 ; 6549 ; 6553 ; 6557 ; 6562 ; 6563 ; 6567 ; 6596 ; 6597 ; 6602 ; 6588 ; 6620 ; 6621 ; 6613 ; 6608 ; 6616 ; 6632 ; 6648 ; 6683 ; 6688 ; 6693 ; 6694 ; 6697 ; 6707 ; 6705 ; 6710 ; 6722 ; 6721 ; 6717 ; 6731 ; 6725 ; 6733 ; 6749 ; 6750 ; 6751 ; 6752 ; 6769 ; 6770 ; 6777 ; 6800 ; 6825 ; 6834 ; 6838 ; 6839 ; 6854 ; 6862 ; 6863 ; 6867 ; 6858 ; 6854 ; 6887 ; 6890 ; 6891 ; 6890 ; 6900 ; 6899 ; 6900 ; 6918 ; 6922 ; 6940 ; 6941 ; 6946 ; 6948 ; 6927 ; 6928 ; 6924 ; 6907 ; 6910 ; 6917 ; 6920 ; 6931 ; 6943 ; 6971 ; 6973 ; 6974 ; 6978 ; 7001 ; 7000 ; 7001 ; 7017 ; 7018 ; 7019 ; 7020 ; 7021 ; 7023 ; 7027 ; 6998 ; 6980 ; 6979 ; 6975 ; 6974 ; 6984 ; 6992 ; 6994 ; 7003 ; 7011 ; 7012 ; 7029 ; 7041 ; 7044 ; 7063 ; 7065 ; 7067 ; 7069 ; 7076 ; 7074 ; 7076 ; 7080 ; 7083 ; 7086 ; 7108 ; 7137 ; 7141 ; 7142 ; 7159 ; 7160 ; 7158 ; 7157 ; 7165 ; 7167 ; 7189 ; 7193 ; 7194 ; 7192 ; 7196 ; 7200 ; 7203 ; 7209 ; 7214 ; 7219 ; 7209 ; 7169 ; 7168 ; 7169 ; 7171 ; 7168 ; 7170 ; 7165 ; 7166 ; 7176 ; 7178 ; 7168 ; 7165 ; 7169 ; 7170 ; 7171 ; 7172 ; 7176 ; 7177 ; 7178 ; 7179 ; 7174 ; 7178 ; 7182 ; 7158 ; 7172 ; 7193 ; 7199 ; 7192 ; 7191 ; 7194 ; 7192 ; 7195 ; 7199 ; 7202 ; 7201 ; 7216 ; 7213 ; 7237 ; 7256 ; 7245 ; 7246 ; 7249 ; 7263 ; 7265 ; 7262 ; 7272 ; 7273 ; 7294 ; 7298 ; 7296 ; 7300 ; 7301 ; 7313 ; 7314 ; 7316 ; 7318 ; 7326 ; 7327 ; 7328 ; 7330 ; 7349 ; 7350 ; 7366 ; 7381 ; 7382 ; 7383 ; 7390 ; 7399 ; 7382 ; 7384 ; 7380 ; 7381 ; 7385 ; 7388 ; 7402 ; 7403 ; 7404 ; 7405 ; 7409 ; 7420 ; 7424 ; 7420 ; 7421 ; 7426 ; 7429 ; 7435 ; 7425 ; 7426 ; 7429 ; 7452 ; 7455 ; 7474 ; 7469 ; 7472 ; 7473 ; 7466 ; 7461 ; 7462 ; 7470 ; 7472 ; 7474 ; 7480 ; 7474 ; 7475 ; 7473 ; 7472 ; 7475 ; 7484 ; 7472 ; 7473 ; 7489 ; 7491 ; 7514 ; 7516 ; 7509 ; 7510 ; 7513 ; 7514 ; 7539 ; 7549 ; 7571 ; 7582 ; 7570 ; 7573 ; 7574 ; 7575 ; 7576 ; 7581 ; 7589 ; 7587 ; 7590 ; 7592 ; 7598 ; 7625 ; 7627 ; 7626 ; 7627 ; 7633 ; 7638 ; 7640 ; 7641 ; 7645 ; 7657 ; 7669 ; 7670 ; 7654 ; 7681 ; 7682 ; 7679 ; 7689 ; 7690 ; 7696 ; 7709 ; 7718 ; 7723 ; 7721 ; 7722 ; 7728 ; 7751 ; 7764 ; 7756 ; 7747 ; 7748 ; 7757 ; 7766 ; 7779 ; 7784 ; 7785 ; 7804 ; 7805 ; 7808 ; 7809 ; 7808 ; 7834 ; 7836 ; 7837 ; 7853 ; 7854 ; 7855 ; 7866 ; 7901 ; 7920 ; 7930 ; 7931 ; 7928 ; 7931 ; 7916 ; 7917 ; 7947 ; 7948 ; 7952 ; 7948 ; 7985 ; 7984 ; 7988 ; 7998 ; 8004 ; 8017 ; 8016 ; 8033 ; 8035 ; 8039 ; 8053 ; 8062 ; 8063 ; 8064 ; 8068 ; 8085 ; 8104 ; 8103 ; 8104 ; 8107 ; 8113 ; 8116 ; 8127 ; 8129 ; 8119 ; 8117 ; 8139 ; 8140 ; 8142 ; 8151 ; 8153 ; 8168 ; 8169 ; 8172 ; 8161 ; 8162 ; 8165 ; 8168 ; 8176 ; 8180 ; 8182 ; 8188 ; 8196 ; 8197 ; 8209 ; 8211 ; 8212 ; 8219 ; 8222 ; 8187 ; 8191 ; 8192 ; 8207 ; 8208 ; 8211 ; 8213 ; 8196 ; 8207 ; 8215 ; 8214 ; 8217 ; 8207 ; 8208 ; 8209 ; 8228 ; 8233 ; 8241 ; 8245 ; 8257 ; 8258 ; 8257 ; 8258 ; 8259 ; 8263 ; 8269 ; 8273 ; 8276 ; 8274 ; 8269 ; 8276 ; 8279 ; 8280 ; 8267 ; 8263 ; 8266 ; 8264 ; 8266 ; 8267 ; 8270 ; 8272 ; 8271 ; 8280 ; 8286 ; 8296 ; 8312 ; 8314 ; 8320 ; 8322 ; 8335 ; 8339 ; 8340 ; 8338 ; 8367 ; 8401 ; 8415 ; 8416 ; 8419 ; 8420 ; 8426 ; 8427 ; 8425 ; 8440 ; 8438 ; 8447 ; 8446 ; 8453 ; 8486 ; 8492 ; 8488 ; 8503 ; 8512 ; 8525 ; 8523 ; 8508 ; 8520 ; 8522 ; 8547 ; 8552 ; 8542 ; 8541 ; 8561 ; 8562 ; 8583 ; 8584 ; 8585 ; 8586 ; 8594 ; 8593 ].

Fixpoint _count_gt (head : nat) (tail : list nat) :=
    match tail with
    | [] => 0
    | y :: xs => match (y <=? head) with
        | false => 1 + _count_gt y xs
        | true => _count_gt y xs
        end
    end.

Definition count_gt (data : list nat) :=
    match (head data) with
    | Some head => Some (_count_gt head (tail data))
    | None => None
    end.

Fixpoint compute_sliding (data : list nat) :=
    match data with
    | [] => []
    | x :: l =>
        match l with
        | [] => []
        | y :: l' =>
            match l' with
            | [] => []
            | z :: _ =>
                x + y + z :: compute_sliding l
            end
        end
    end.

Definition part_1 := count_gt data.
Definition part_2 := count_gt (compute_sliding data).

Compute part_1.
Compute part_2.