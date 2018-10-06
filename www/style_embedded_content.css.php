/***********/
/* SIDEBAR */
/***********/

#content > #primary-bar,
#content > #secondary-bar {
	grid-column: 1 / span 4;
}
#secondary-content-column {
	grid-column: 4;
	grid-row: 3 / span 1000;
	display: flex;
	flex-flow: column;
	margin: 0 -30px 0 30px;
	box-shadow:
		1px 0 0 0 #ddd inset;
}
#secondary-content-column .content-area {
	padding: 0 1px 0 2px;
	display: flex;
	flex-flow: column;
	border-style: solid;
	border-color: #ddd;
	border-width: 0 0 1px 0;
}
#secondary-content-column .content-area.recent-posts {
	height: 500px;
}
#secondary-content-column .content-area.recent-comments {
	height: 848px;
}
#secondary-content-column .content-area object {
	height: 100%;
}
#secondary-content-column .content-area > h1 {
	margin: 0.75em 0 0.375em 0;
	font-size: 1.5rem;
	font-weight: 400;
	line-height: 1;
	text-align: center;
	color: #999;
}
#secondary-content-column .content-area > p {
	text-align: right;
	margin: 0;
	padding: 4px 16px;
	font-size: 1.25em;
}
#secondary-content-column .content-area > p a {
	color: #999;
}
