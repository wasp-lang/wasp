import addWaspSourceHeader from './addWaspSourceHeader'
import './Layout.css'
import TagsSidebar from './TagsSidebar'
import TopNavbar from './TopNavbar'

const Layout = ({ user, activeTag, children }) => (
  <div className='layout-root'>
    <TopNavbar user={user} />
    <div className='layout-content'>
      <TagsSidebar active={activeTag} />
      {children}
    </div>
  </div>
)

export default addWaspSourceHeader(Layout)
