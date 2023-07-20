
## Sort gtsummary tab
moreflex <- function(tab) {
	tab <- (tab
		%>% as_flex_table()
		%>% flextab_fit()
	)
	return(tab)
}

crosstab_sort <- function(object, decreasing=FALSE, ...) {
	temp <- (object$table_body
		%>% mutate(.stort_var=as.numeric(gsub("\\ \\(.*", "", stat_0)))
	)
	if (decreasing) {
		temp <- (temp
			%>% arrange(!is.na(.stort_var), desc(.stort_var))
		)
	} else {
		temp <- (temp
			%>% arrange(!is.na(.stort_var), .stort_var)
		)
	}
	temp <- temp %>% select(-.stort_var)
	object$table_body <- temp
	return(object)
}

## Sort gt summary tab
summarytab_sort <- function(object, decreasing=FALSE, ...) {
	temp <- (object$table_body
		%>% mutate(.stort_var=as.numeric(gsub("\\ \\(.*", "", stat_1)))
	)
	if (decreasing) {
		temp <- (temp
			%>% group_by(var_label)
			%>% arrange(!is.na(.stort_var), desc(.stort_var))
			%>% ungroup()
		)
	} else {
		temp <- (temp
			%>% group_by(var_label)
			%>% arrange(!is.na(.stort_var), .stort_var)
			%>% ungroup()
		)
	}
	temp <- temp %>% select(-.stort_var)
	object$table_body <- temp
	return(object)
}

## Fit flexi table on a pdf page
flextab_fit <- function(ft, pgwidth = 6){
	ft_out <- ft %>% autofit()
	ft_out <- width(ft_out, width = dim(ft_out)$widths*pgwidth/(flextable_dim(ft_out)$widths))
	return(ft_out)
}

## Add multiple response labels to 0/1 labels
addmultiLabs <- function(lab_df, var) {
	out <- (lab_df
		%>% filter(variables==var)
		%>% pull(labels)
	)
	out2 <- "1"
	names(out2) <- out
	out2 <- c(NULL="0", out2)
	return(out2)
}

## table1 extended functions
asdataframe <- function (x, ...) {
    obj <- attr(x, "obj")
    with(obj, {
        rlh <- if (is.null(rowlabelhead) || rowlabelhead == "")
            " "
        else rowlabelhead
        z <- lapply(contents, function(y) {
            y <- as.data.frame(y, stringsAsFactors = F)
            y2 <- data.frame(x = paste0(c("", rep("  ", nrow(y) -
                1)), rownames(y)), stringsAsFactors = F)
            y <- cbind(setNames(y2, rlh), y)
            y
        })
        df <- do.call(rbind, z)
        df <- rbind(c("", ifelse(is.na(headings[2, ]), "", sprintf("(N=%s)",
            headings[2, ]))), df)
        if (!"overall" %in% names(headings[1,])) {
            df <- rbind(c("", headings[1,]), df)
            colnames(df) <- c(rlh, gsub(paste0(headings[1,], "\\.", collapse = "|"), "", names(headings[1, ])))
        }
        colnames(df) <- c(rlh, headings[1, ])
        rownames(df) <- NULL
        noquote(df)
    })
}

table1fun <- function(data, x, group="Round", cap.var=x, cap=NULL, save.excel=TRUE, wb=NULL, filename=NULL, ...) {
	form <- as.formula(paste0("~", paste0(x, collapse="+"), "|", paste0(group, collapse="*")))
	if (is.null(cap)) {
		cap <- paste0(attr(data[[cap.var]], "label"), " by ", group)
	}
	tab <- table1(form, data=data, caption=cap, ...)
	
	if (save.excel) {
		saveExcel(tab=asdataframe(tab), sheet=cap.var, caption=cap, wb=wb, filename=filename)
		out <- list(render=tab, caption=cap, var=cap.var)
	} else {
		out <- list(render=tab, excel=asdataframe(tab), caption=cap, var=cap.var)
	}
	out
}

saveExcel <- function(tab, sheet=tab$var, caption=tab$caption, wb, filename, ...) {
	my_headline <- createStyle(textDecoration = "Bold",fontSize = 14)
	addWorksheet(wb, sheet)
	writeData(wb, sheet, caption, startCol = 1, startRow = 1, rowNames = FALSE)
	tab_df <- tab$excel
	if (is.null(tab_df)) {
		writeData(wb, sheet, tab, startCol = 1, startRow = 2, rowNames = FALSE,  headerStyle=my_headline)
	} else {
		writeData(wb, sheet, tab$excel, startCol = 1, startRow = 2, rowNames = FALSE,  headerStyle=my_headline)
	}
	saveWorkbook(wb, paste0(gsub("\\.xlsx$|\\.pptx$", "", filename), ".xlsx"), overwrite = TRUE)
}


asTibble <- function(tab, ...) {
	tab1 <- as_tibble(tab, col_labels = TRUE)
	header <- (tab$table_styling$header
		%>% dplyr::filter(!hide)
		%>% mutate(spanning_header=ifelse(is.na(spanning_header), " ", as.character(gsub("\\**", "", spanning_header))))
	)
	new_header <- gsub("\\**", "", header$label)
	names(new_header) <- header$spanning_header
	tab1 <- rbind(new_header, tab1)
	colnames(tab1) <- names(new_header)
	noquote(tab1)
}

generateTab <- function(df, vars, by, strata=NULL, missing="no"
	, type=NULL, test=NULL, percent="row", group=NULL
	, test.args=NULL, add.p=TRUE, add.ci=TRUE
	, statistic = list(all_continuous() ~ "{mean} [{min}, {max}]"
		, all_categorical() ~ "{n} ({p})"
	)
	, value=NULL
	, combine_with = c("tbl_merge")
	, stack_group_header=FALSE
	, filterNA=TRUE
	, add.plot=TRUE
	, save.excel=FALSE
	, focus_level=NULL
	, pos=0.8
	, ..., wb=NULL
	, filename=NULL, sheet=vars[1], caption=NULL) {
	if (!is.null(strata)) {
		allvars <- c(strata, by, vars)
	} else {
		allvars <- c(by, vars)
	}
	if (is.null(caption)) {
		caption <- paste0(attr(df[[vars[1]]], "label"), " by ", attr(df[[by]], "label"))
	}
	tab <- (df
		%>% ungroup()
		%>% select(all_of(allvars))
	)
	if (filterNA) {
		tab <- (tab
			%>% filter(., if_any(all_of(vars), function(x)!is.na(x)))
		)
	}
	tab <- (tab
		%>% sjmisc::to_label(drop.levels=TRUE)
	)
	if (add.plot) {
		p1 <- sapply(1:length(vars), function(x){
			.xlab <- attr(tab[[vars[x]]], "label")
			.slab <- attr(tab[[allvars[[1]]]], "label")
			if (is.null(caption)) {
				.caption <- paste0(.xlab, " by ", attr(tab[[by]], "label"))
			} else {
				.caption=caption
			}
			p1 <- addbarPlot(tab
				, var=vars[[x]]
				, by=by
				, strata=strata
				, percent=percent
				, focus_level=focus_level
				, pos=pos
			)
			p1 <- (p1
				+ labs(x=.slab, fill=attr(tab[[by]], "label"), y="", title=.caption)
			)
			pdf(paste0("outputs/", vars[[x]], ".pdf"), height=6)
			print(p1)
			dev.off()
			
			png(paste0("outputs/", vars[[x]], ".png"), width=800, heigh =500, units="px")
			print(p1)
			dev.off()

			## Save powerpoint
			create_pptx(p1, path=paste0(gsub("\\.xlsx$|\\.pptx$", "", filename), ".pptx"))

			return(p1)
		}, simplify=FALSE)
	} else {
		p1 <- NULL
	}
	
	ttfun <- function(tab) {
		tab <- (tab	
			%>% tbl_summary(
				by = all_of(by)
				, statistic = statistic
				, missing = missing
				, type=type
				, value=value
				, percent=percent
				, sort = all_categorical() ~ "frequency"
			)
		)
		if (add.ci) {
		tab <- (tab
			%>% add_ci(statistic=list(all_categorical() ~ "{conf.low}, {conf.high}"
				, all_continuous() ~ "{conf.low}, {conf.high}")
			)
		)
		}
		if (add.p) {
			tab <- tab %>% add_p(test=test, group=group, test.args=test.args)
		}
		tab <- (tab
			%>% add_overall()
#			%>% crosstab_sort(decreasing=TRUE)
			%>% modify_header(label = "**Variable**")
			%>% modify_caption(paste0("**", caption, "**"))
		)
		return(tab)
	}

	if (!is.null(strata)) {
		tab <- (tab
			%>% tbl_strata(
				strata=all_of(strata),
				.tbl_fun=
					~ .x
					%>% ttfun()
				, .combine_with=combine_with
				, .stack_group_header=stack_group_header
			)
		)
	} else {
		tab <- (tab
			%>% ttfun()
		)
	}
	if (save.excel) {
		if (is.null(strata)) {
			tab_df <- as_tibble(tab)
			colnames(tab_df) <- gsub("\\**", "", colnames(tab_df))
		} else {
			tab_df <- asTibble(tab)
		}
		saveExcel(tab=tab_df, sheet=sheet, caption=caption, wb=wb, filename=filename)
	}

	out <- list(tab=tab, plot=p1)
	return(out)
}


addbarPlot <- function(df, var, by, strata=NULL, percent="row", focus_level=NULL, pos=0.8, ...) {
	if (!is.null(strata)) {
		allvars <- c(strata, by, var)
	} else {
		allvars <- c(by, var)
	}
	main_var <- last(allvars)
	strata <- first(allvars)
	oldlevels <- levels(df[[strata]])
	dat <- (df
		%>% filter_at(main_var, any_vars(!is.na(.)))
		%>% group_by_at(allvars)
		%>% summarise(n = n(), .groups = "drop_last")
		%>% mutate(col_pct=round(n/sum(n, na.rm=TRUE)*100, 1))
		%>% ungroup()
		%>% group_by_at(c(strata, main_var))
		%>% mutate(row_pct=round(n/sum(n, na.rm = TRUE)*100, 1))
		%>% mutate(N=sum(n, na.rm = TRUE)) # Comment this to do overall N below
		%>% ungroup()
#		%>% group_by_at(first(allvars))  # Uncomment these two lines for overall N
#		%>% mutate(N=sum(n, na.rm = TRUE))
	)
	if (!is.null(focus_level)) {
		dat <- (dat
			%>% filter_at(main_var, any_vars(.==focus_level))
		)
		fill_var <- strata
		legend_add <- theme(legend.position="none")
	} else {
		fill_var <- main_var
		legend_add <- theme(legend.position="bottom")
	}
	dat[[strata]] <- as.factor(paste0(dat[[strata]], " (N=", dat[["N"]],")"))
	newlevels <- levels(dat[[strata]])
	newlevels <- newlevels[match(oldlevels, gsub("\\ \\(.*", "", newlevels), nomatch=FALSE)]
	dat[[strata]] <- factor(dat[[strata]], levels=newlevels, labels=newlevels)
	
	p1 <- ggplot(dat, aes_string(x=strata, fill=fill_var))
	if (percent=="row") {
		p1 <- (p1 + geom_bar(aes(y=row_pct), stat = "identity", position = position_dodge(width=pos))
			+ geom_text(aes(label=row_pct, y=row_pct), position = position_dodge(width=pos), color="black", size=3)
		)
	} else {
		p1 <- (p1 + geom_bar(aes(y=col_pct), stat = "identity", position = position_dodge(width=pos))
			+ geom_text(aes(label=col_pct, y=col_pct), position = position_dodge(width=pos), vjust=2, color="black", size=3.8)
		)
	}

	p1 <- (p1
		+ scale_fill_brewer(palette="Dark2")
		+ facet_wrap(as.formula(paste0("~", by)), scales="free_x", drop=TRUE)
		+ legend_add
	)
	return(p1)
}

## Walds CI
waldCIfun <- function(x, exponentiate =  FALSE, conf.level = 0.95, ...) {
  dplyr::bind_cols(
    broom::tidy(x, exponentiate = exponentiate, conf.int = FALSE),
    # calculate the confidence intervals, and save them in a tibble
    stats::confint.default(x) %>%
      tibble::as_tibble() %>%
      rlang::set_names(c("conf.low", "conf.high"))  )
}

## Format large numbers
formatfun <- function(x, digits=2) {
	round(x, digits)
}

