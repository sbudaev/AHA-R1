#!/usr/bin/wish
# This is a template to produce the Genotype x Phenotype matrix for
# specific number of chromosomes (first parameter of gpmatrix)
# and maximum genome size (second parameter of gpmatrix)
# ------------------------------------------------------------------------------

# gpmatrix: This procedure produces the G x P Matrix code for two arguments:
# N of chromosomes and max N of alleles.
proc gpmatrix { n_chrom n_max_genome_size } {
  # n_chrom is the number of shromosomes
  # n_max_genome_size is the maximum genome size, i.e. the maximum number
  #                   of alleles in these chromosomes.
  set out    "  logical, dimension(MAX_NALLELES,N_CHROMOSOMES),parameter,public             & \n"
  append out "        :: XXXX_GENOTYPE_NEURONAL = reshape (                                 & \n"
  append out "  ! ............................................................................\n"

  append out "                    \[ &   !"
  for {set i 1} {$i <= $n_chrom} {incr i} {
    append out "    " $i
  }
  append out "\n"

  append out "                    !       "
  for {set i 1} {$i <= $n_chrom} {incr i} {
    append out "-----"
  }
  append out "\n"

  for {set i 1 } {$i < $n_max_genome_size} {incr i 1} {
    append out "                            "
    for {set j 1} {$j <= $n_chrom} {incr j} {
      append out "  NO,"
    }
    append out "   & !  " $i "\n"
  }

  append out "                            "
  for {set j 1} {$j < $n_chrom} {incr j} {
    append out "  NO,"
  }
  append out "  NO \], & !  " $i "\n"

  append out "                    !       "
  for {set i 1} {$i <= $n_chrom} {incr i} {
    append out "-----"
  }
  append out "\n"
  append out "                    !  max.: \n"
  append out "                    \[MAX_NALLELES,N_CHROMOSOMES\], \[NO\], \[2,1\] ) \n"
  append out "  ! Additional reshape params:    array shape    | pad | order \n"
  append out "  !                                              | with| by rows \n"
  append out "  ! ............................................................................\n"
  return $out
}

# get_info: This procedure prints a brief help into the output screen.
proc get_info {} {
  .out insert end "Brief help:\n\nThis utility produces Fortran code template for the Genotype x Phenotype matrix.\n\n"
  .out insert end "Select N of chromosomes and N of alleles and press \[Update code\] to get the matrix template.\n"
  .out insert end "To print matrix to std.output press \[Send to terminal\].\n"
  .out insert end "\[Clean\] cleans the output screen. \[Exit\] exits this program."
}

# ------------------------------------------------------------------------------

set chrom_n 7
set allele_n 12

wm title . "AHA Model: Genotype x Phenotype matrix template"

label .lab -width 60 -text "Generate Fortran code template for the Genotype x phenotype matrix" -anchor w

frame .fields -width 80 -height 40
label .l_chrom -text "N chromosomes: "
label .l_allel -text "N alleles (max): "
spinbox .e_n_chrom -textvariable chrom_n -from 4 -to 18 -increment 1 -width 3
spinbox .e_n_allel -textvariable allele_n -from 4 -to 24 -increment 1 -width 3
button .bt_do -text "Update code" -bg red -command {
       .out insert 1.0 [ .out delete 0.0 end; gpmatrix $chrom_n $allele_n ] };
button .bt_pr -text "Send to terminal" -command { puts [ gpmatrix $chrom_n $allele_n ] };
button .bt_cl -text "Clean" -command { .out delete 0.0 end };
button .bt_hl -text "?" -command { .out delete 0.0 end ; get_info };

grid .l_chrom .e_n_chrom  .l_allel .e_n_allel .bt_do .bt_pr .bt_cl .bt_hl -in .fields -padx 4 -pady 4

frame .txt -width 100

text .out -yscrollcommand {.sbar set} -width 100 -wrap none
scrollbar .sbar -command {.out yview}

grid .out .sbar -in .txt -sticky nsew

get_info

button .bt_exit -text "Exit" -command exit;

pack .lab -padx 40 -pady 10
pack .fields
pack .txt
pack .bt_exit -padx 40 -pady 10
