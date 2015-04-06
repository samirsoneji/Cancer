lt <- read.table("~/Cancer/data/bltper_5x1.txt",skip=2,header=TRUE)
year.select <- 2010
ax <- lt[lt$Year==year.select,"ax"]
ax <- ax[1:22]
save(ax, file="~/Cancer/data/ax.Rdata")
