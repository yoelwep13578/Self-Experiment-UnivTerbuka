library("cli")
library("ggplot2")
library("ggforce")

# Variabel Inisiasi
nama_bulan <- c("Januari", "Februari", "Maret", "April", "Mei", "Juni", "Juli", "Agustus", "September", "Oktober", "November", "Desember")
nama_bulan_3_huruf <- c("Jan", "Feb", "Mar", "Apr", "Mei", "Jun", "Jul", "Agu", "Sep", "Okt", "Nov", "Des")

varian_sepatu <- c("Hitam", "Putih", "Biru")

color_soft_black <- "#555555"
color_soft_white <- "#CCCCCC"
color_soft_blue <- "#4A90E2"

color_light_black <- "#C7C7C7"
color_light_white <- "#EEEEEE"
color_light_blue <- "#BBC1F0"


# File CSV
spreadsheet_penjualan_sepatu_niki <- read.csv("./data-csv/data-penjualan-sepatu.csv", header = TRUE)

# Rename Column
colnames(spreadsheet_penjualan_sepatu_niki) <- c("Bulan", "Sepatu_Hitam_Terjual", "Sepatu_Putih_Terjual", "Sepatu_Biru_Terjual")

# Column Pendapatan
harga_sepatu_niki <- 100000

spreadsheet_penjualan_sepatu_niki$Pendapatan_Sepatu_Hitam <- spreadsheet_penjualan_sepatu_niki$Sepatu_Hitam_Terjual * harga_sepatu_niki
spreadsheet_penjualan_sepatu_niki$Pendapatan_Sepatu_Putih <- spreadsheet_penjualan_sepatu_niki$Sepatu_Putih_Terjual * harga_sepatu_niki
spreadsheet_penjualan_sepatu_niki$Pendapatan_Sepatu_Biru <- spreadsheet_penjualan_sepatu_niki$Sepatu_Biru_Terjual * harga_sepatu_niki

# Total Penjualan dan Pendapatan
total_penjualan_sepatu_hitam <- sum(spreadsheet_penjualan_sepatu_niki$Sepatu_Hitam_Terjual)
total_penjualan_sepatu_putih <- sum(spreadsheet_penjualan_sepatu_niki$Sepatu_Putih_Terjual)
total_penjualan_sepatu_biru <- sum(spreadsheet_penjualan_sepatu_niki$Sepatu_Biru_Terjual)
total_penjualan_sepatu <- sum(total_penjualan_sepatu_hitam, total_penjualan_sepatu_putih, total_penjualan_sepatu_biru)

total_pendapatan_sepatu_hitam <- sum(spreadsheet_penjualan_sepatu_niki$Pendapatan_Sepatu_Hitam)
total_pendapatan_sepatu_putih <- sum(spreadsheet_penjualan_sepatu_niki$Pendapatan_Sepatu_Putih)
total_pendapatan_sepatu_biru <- sum(spreadsheet_penjualan_sepatu_niki$Pendapatan_Sepatu_Biru)
total_pendapatan_sepatu <- sum(total_pendapatan_sepatu_hitam, total_pendapatan_sepatu_putih, total_pendapatan_sepatu_biru)

persentase_penjualan_sepatu_hitam <- round((total_penjualan_sepatu_hitam / total_penjualan_sepatu) * 100, 1)
persentase_penjualan_sepatu_putih <- round((total_penjualan_sepatu_putih / total_penjualan_sepatu) * 100, 1)
persentase_penjualan_sepatu_biru <- round((total_penjualan_sepatu_biru / total_penjualan_sepatu) * 100, 1)

# Keuangan Perusahaan
total_biaya_produksi <- 5000000000


# -------- #
# Function #
# -------- #

# Formatting
cell_number_separator <- function(angka) {
    format(angka, big.mark = ".", decimal.mark = ",", scientific = FALSE)
}

cell_rupiah_currency <- function(harga) {
    paste("Rp", cell_number_separator(harga), sep = "")
}

# Salinan Tabel untuk CLI
tabel_lengkap_penjualan_sepatu_niki <- function() {
    tabel_penjualan_sepatu_niki <- spreadsheet_penjualan_sepatu_niki
    tabel_penjualan_sepatu_niki <- rbind(tabel_penjualan_sepatu_niki, data.frame(
        Bulan = "Total",
        Sepatu_Hitam_Terjual = total_penjualan_sepatu_hitam,
        Sepatu_Putih_Terjual = total_penjualan_sepatu_putih,
        Sepatu_Biru_Terjual = total_penjualan_sepatu_biru,
        Pendapatan_Sepatu_Hitam = total_pendapatan_sepatu_hitam,
        Pendapatan_Sepatu_Putih = total_pendapatan_sepatu_putih,
        Pendapatan_Sepatu_Biru = total_pendapatan_sepatu_biru))

        tabel_penjualan_sepatu_niki$Sepatu_Hitam_Terjual <- cell_number_separator(tabel_penjualan_sepatu_niki$Sepatu_Hitam_Terjual)
        tabel_penjualan_sepatu_niki$Sepatu_Putih_Terjual <- cell_number_separator(tabel_penjualan_sepatu_niki$Sepatu_Putih_Terjual)
        tabel_penjualan_sepatu_niki$Sepatu_Biru_Terjual <- cell_number_separator(tabel_penjualan_sepatu_niki$Sepatu_Biru_Terjual)
        tabel_penjualan_sepatu_niki$Pendapatan_Sepatu_Hitam <- cell_rupiah_currency(tabel_penjualan_sepatu_niki$Pendapatan_Sepatu_Hitam)
        tabel_penjualan_sepatu_niki$Pendapatan_Sepatu_Putih <- cell_rupiah_currency(tabel_penjualan_sepatu_niki$Pendapatan_Sepatu_Putih)
        tabel_penjualan_sepatu_niki$Pendapatan_Sepatu_Biru <- cell_rupiah_currency(tabel_penjualan_sepatu_niki$Pendapatan_Sepatu_Biru)

    colnames(tabel_penjualan_sepatu_niki) <- c("Bulan", "Sepatu Hitam Terjual", "Sepatu Putih Terjual", "Sepatu Biru Terjual", "Pendapatan Sepatu Hitam", "Pendapatan Sepatu Putih", "Pendapatan Sepatu Biru")
    return(tabel_penjualan_sepatu_niki)
}

# Grafik Garis Progres Penjualan Sepatu Niki
grafik_progres_penjualan_sepatu_niki <- function() {
    grafik_progres_penjualan_sepatu_niki <- ggplot(spreadsheet_penjualan_sepatu_niki, aes(x = factor(Bulan, levels = nama_bulan))) +
        geom_line(aes(y = Sepatu_Hitam_Terjual, color = "Hitam", group = 1)) +
        geom_point(aes(y = Sepatu_Hitam_Terjual, color = "Hitam")) +
        geom_line(aes(y = Sepatu_Putih_Terjual, color = "Putih", group = 2)) +
        geom_point(aes(y = Sepatu_Putih_Terjual, color = "Putih")) +
        geom_line(aes(y = Sepatu_Biru_Terjual, color = "Biru", group = 3)) +
        geom_point(aes(y = Sepatu_Biru_Terjual, color = "Biru")) +
        labs(title = "Progres Penjualan Sepatu Niki",
             x = "Bulan",
             y = "Jumlah Sepatu Terjual",
             color = "Varian Sepatu") +
        scale_x_discrete(labels = nama_bulan_3_huruf) +
        scale_color_manual(values = c(color_soft_blue, color_soft_black, color_soft_white)) +
        theme_minimal() +
        theme(text = element_text(family = "Radio Canada"))

    return(grafik_progres_penjualan_sepatu_niki)
}

# Grafik Pie Perbandingan Varian Sepatu
grafik_perbandingan_varian_sepatu <- function() {
    perbandingan_varian_sepatu <- data.frame(
        Varian_Sepatu = varian_sepatu,
        Total_Penjualan = c(total_penjualan_sepatu_hitam, total_penjualan_sepatu_putih, total_penjualan_sepatu_biru),
        Persentase = cell_number_separator(c(persentase_penjualan_sepatu_hitam, persentase_penjualan_sepatu_putih, persentase_penjualan_sepatu_biru))
    )

    pie_chart <- ggplot(perbandingan_varian_sepatu, aes(x = "", y = Total_Penjualan, fill = Varian_Sepatu)) +
        geom_bar(stat = "identity", width = 1, color = "black", linewidth = 0.25) +
        coord_polar("y", start = 0) +
        labs(title = "Minat Konsumen terhadap Varian Sepatu",
             fill = "Varian Sepatu") +
        geom_text(aes(label = paste0(Varian_Sepatu, " (", Persentase, "%", ")")), position = position_stack(vjust = 0.5)) +
        scale_fill_manual(values = c(color_light_blue, color_light_black, color_light_white)) +
        geom_circle(aes(x0 = 0, y0 = 0, r = 0.3), fill = "white", color = NA) +
        theme_void() +
        theme(text = element_text(family = "Radio Canada"))

    return(pie_chart)
}

# Kondisi Keuangan Perusahaan
kondisi_keuangan_perusahaan <- function() {
    cli_text("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
    cli_text(paste0("Total Biaya Produksi\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0: ", cell_rupiah_currency(total_biaya_produksi)))
    cli_text(paste0("Pendapatan yang Diperoleh : ", cell_rupiah_currency(total_pendapatan_sepatu)))
    cli_text("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")

    selisih <- total_pendapatan_sepatu - total_biaya_produksi
    persentase <- cell_number_separator(round(abs(selisih) / total_biaya_produksi * 100, 2))

    if (total_biaya_produksi > total_pendapatan_sepatu) {
        status <- paste0("{.down Rugi (", persentase, "%)}")
        selisih <- paste0("{.down ", cell_rupiah_currency(selisih), "}")
    }
    else if (total_biaya_produksi == total_pendapatan_sepatu) {
        status <- paste0("{.mid Impas (", persentase, "%)}")
        selisih <- paste0("{.mid ", cell_rupiah_currency(selisih), "}")
    }
    else {
        status <- paste0("{.up Untung (", persentase, "%)}")
        selisih <- paste0("{.up ", cell_rupiah_currency(selisih), "}")
    }

    cli_text(paste0("Status Keuangan\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0: ", status))
    cli_text(paste0("Selisih Laba/Rugi\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0: ", selisih))
    cli_text("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
    cli_text("")

}

# Konfirmasi Lanjut
step_confirm <- function() {
    cli_text("")
    cli_div(theme = list(span.continue = list(color = "green"), span.terminate = list(color = "orange")))
    cli_alert_info("Pencet {.continue [ENTER]} buat lanjutin, atau pencet {.terminate [CTRL][C]} kalo mau udahan.")
    cli_end()
    readline()
}

# -------------- #
# Output Program #
# -------------- #

cli_div(theme = list(span.up = list(color = "green"), span.mid = list(color = "yellow"), span.down = list(color = "orange")))

cli_h1("Laporan Penjualan Sepatu Niki")
cli_text("")
cli_text("Saat ini kamu lagi jalanin program R yang berisi laporan penjualan & perolehan sepatu Niki, serta keuangan dari perusahaan Niki. Habis ini akan menampilkan Tabel Laporan Penjualan dan Pendapatan Sepatu Niki Pada Tahun 2024 dalam bentuk CLI di terminal ini.")
step_confirm()

cli_h2("Penjualan dan Penghasilan Sepatu Niki")
print(tabel_lengkap_penjualan_sepatu_niki())
cli_text("")
cli_text("Laporan ini menyajikan informasi secara rinci dan difokuskan pada aktivitas operasional perusahaan Niki. Berdasarkan laporan ini, tampaknya {.emph angka progres penjualan sepatu Niki varian hitam semakin meningkat} daripada varian lain. Penjualan sepatu {.up varian hitam selalu naik secara konsisten dari bulan Januari hingga Desember}. Hal ini berbeda pada {.mid varian lain yang progres penjualannya naik-turun}.")
cli_text("")
cli_text("Habis ini akan menampilkan tabel yang sama, tapi dalam bentuk GUI di window lain.")
step_confirm()

cli_h2("Penjualan dan Penghasilan Sepatu Niki (GUI)")
View(tabel_lengkap_penjualan_sepatu_niki())
cli_text("Laporan ini sama aja kayak sebelumnya. Sekarang udah ditampilin di window lain. Silakan cek di sana!")
cli_text("")
cli_text("Habis ini akan menampilkan Grafik Progres Penjualan Sepatu.")
step_confirm()

cli_h2("Grafik Progres Penjualan Sepatu Niki")
print(grafik_progres_penjualan_sepatu_niki())
cli_text("Laporan ini menyajikan informasi secara rinci dan difokuskan pada aktivitas operasional perusahaan Niki. Berdasarkan laporan ini, tampaknya {.emph progres penjualan sepatu Niki varian hitam lebih mendominasi} daripada varian lain. Penjualan sepatu {.up varian hitam selalu naik secara konsisten dari bulan Januari hingga Desember}. Hal ini berbeda pada {.mid varian lain yang progres penjualannya naik-turun}.")
cli_text("")
cli_text("Habis ini akan menampilkan Grafik Varian Sepatu Terfavorit Menurut Konsumen.")
step_confirm()

cli_h2("Grafik Varian Sepatu Terfavorit")
print(grafik_perbandingan_varian_sepatu())
cli_text("Laporan ini menyajikan informasi yang terfokus pada perbandingan varian sepatu yang diincar oleh konsumen. Dari sini kelihatan bahwa {.emph sepatu varian hitam sangat diincar oleh konsumen} —— {.up mendominasi hingga 48%}. Urutan kedua diduduki oleh varian putih, akan tetapi {.down konsumen kurang begitu suka dengan varian biru —— yakni hanya 20%}.")
cli_text("")
cli_text("Habis ini akan menampilkan Kondisi Keuangan Perusahaan di terminal ini.")
step_confirm()

cli_h2("Kondisi Keuangan Perusahaan")
kondisi_keuangan_perusahaan()
cli_text("Program ini telah selesai.")

cli_end()
