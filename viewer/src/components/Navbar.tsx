import * as React from 'react'

import { Link, NavLink } from 'react-router-dom'

import UserTab from './Layout/UserTab'

const Navbar = () => (
  <nav className={'navbar navbar-default navbar-static-top'}>
    <div className="container">
      <div className="navbar-header">
        <button type="button" className="navbar-toggle" data-toggle="collapse" data-target="#navbar">
          <span className="sr-only">Toggle navigation</span>
          <span className="icon-bar"></span>
          <span className="icon-bar"></span>
          <span className="icon-bar"></span>
        </button>
        <Link to="/" className="navbar-brand">π-Base</Link>
      </div>
      <div id="navbar" className="navbar-collapse collapse">
        <ul className="nav navbar-nav">
          <li><NavLink activeClassName="active" to="/spaces">Spaces</NavLink></li>
          <li><NavLink activeClassName="active" to="/properties">Properties</NavLink></li>
          <li><NavLink activeClassName="active" to="/theorems">Theorems</NavLink></li>
        </ul>
        <UserTab />
      </div>
    </div>
  </nav>
)

export default Navbar
