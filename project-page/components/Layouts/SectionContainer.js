import classNames from 'classnames'

const SectionContainer = ({ children, className }) => (
  <div 
    className={classNames(
      'container mx-auto px-6 xl:px-20',
      className
    )}
  >
    { children }
  </div>
)

export default SectionContainer
