require(ggplot2)
require(ggpubr)

plot_activation_function <- function(f, title, range){
  ggplot(data.frame(x=range), mapping=aes(x=x)) + 
    geom_hline(yintercept=0, color='red', alpha=1/4) +
    geom_vline(xintercept=0, color='red', alpha=1/4) +
    stat_function(fun=f, colour = "dodgerblue3") +
    ggtitle(title) + theme_bw() +
    scale_x_continuous(name='x') +
    scale_y_continuous(name='f(x)') +
    theme(plot.title = element_text(hjust = 0.5))
}


f <- function(x) {x}
ident = plot_activation_function(f,"Identity",c(-4,4))

f <- function(x){   ifelse(x >= 0, 1, 0) }
binary_step  = plot_activation_function(f,"Binary step",c(-4,4))

f <- function(x){1 / (1 + exp(-x))}
sig = plot_activation_function(f, 'Sigmoid', c(-4,4))

tanh_func <- function(x){tanh(x)}
tanH = plot_activation_function(tanh_func, 'tanH', c(-4,4))


rec_lu_func <- function(x){ ifelse(x < 0 , 0, x )}
RELU  = plot_activation_function(rec_lu_func, 'ReLU', c(-4,4))

soft_plus_func <- function(x){ log(1 + exp(x))}
softy = plot_activation_function(soft_plus_func, 'SoftPlus', c(-4,4))


ggarrange(binary_step, ident,
          sig, tanH,
          RELU , softy,
          labels = LETTERS[1:6] ,
          ncol = 2 , nrow=3)

ggsave("Figures/activation.pdf")
